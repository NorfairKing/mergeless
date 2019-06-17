{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A way to synchronise items without merge conflicts.
--
-- This concept has a few requirements:
--
-- * Items must be immutable.
-- * Items must allow for a centrally unique identifier.
-- * Identifiers for items must be generatable in such a way that they are certainly unique.
--
-- Should mutation be a requirement, then it can be build such that it entails deleting the old version and creating a new version that is the modification of the old version.
--
--
-- There are a few obvious candidates for identifiers:
--
-- * incremental identifiers
-- * universally unique identifiers (recommended).
--
--
--
-- The typical setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
-- A central server should operate as follows:
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponse'.
module Data.Mergeless
  ( Added(..)
  , Synced(..)
  , StoreItem(..)
  , Store(..)
  , emptyStore
  , storeSize
  , addItemToStore
  , deleteUnsynced
  , deleteSynced
  , SyncRequest(..)
  , SyncResponse(..)
    -- * Client-side Synchronisation
  , makeSyncRequest
  , mergeSyncResponse
    -- * Server-side Synchronisation
    -- ** General synchronisation
  , SyncProcessor(..)
  , processSyncCustom
    -- ** Synchronisation with a simple central store
  , CentralStore(..)
  , CentralItem(..)
  , processSyncWith
  , processSync
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

-- | A local item of type @a@ that has been added but not synchronised yet
data Added a =
  Added
    { addedValue :: !a
    , addedCreated :: !UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Added a)

instance FromJSON a => FromJSON (Added a) where
  parseJSON = withObject "Added" $ \o -> Added <$> o .: "value" <*> o .: "added"

instance ToJSON a => ToJSON (Added a) where
  toJSON Added {..} = object ["value" .= addedValue, "added" .= addedCreated]

-- | A local item of type @a@ with an identifier of type @a@ that has been synchronised
data Synced i a =
  Synced
    { syncedUuid :: i
    , syncedValue :: !a
    , syncedCreated :: !UTCTime
    , syncedSynced :: !UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a) => Validity (Synced i a)

instance (FromJSON i, FromJSON a) => FromJSON (Synced i a) where
  parseJSON =
    withObject "Synced" $ \o ->
      Synced <$> o .: "id" <*> o .: "value" <*> o .: "created" <*> o .: "synced"

instance (ToJSON i, ToJSON a) => ToJSON (Synced i a) where
  toJSON Synced {..} =
    object
      [ "id" .= syncedUuid
      , "value" .= syncedValue
      , "created" .= syncedCreated
      , "synced" .= syncedSynced
      ]

-- | A store item with an Id of type @i@ and a value of type @a@
data StoreItem i a
  = UnsyncedItem !(Added a) -- ^ A local item that has not been synchronised to the central store yet
  | SyncedItem !(Synced i a) -- ^ A local item that has been synchronised to the central store already
  | UndeletedItem !i -- ^ An item that has been synchronised to the central store, was subsequently deleted locally but this deletion has not been synchronised to the central store yet.
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a) => Validity (StoreItem i a)

instance (FromJSON i, FromJSON a) => FromJSON (StoreItem i a) where
  parseJSON v =
    (SyncedItem <$> parseJSON v) <|> (UnsyncedItem <$> parseJSON v) <|>
    (UndeletedItem <$> parseJSON v)

instance (ToJSON i, ToJSON a) => ToJSON (StoreItem i a) where
  toJSON (UnsyncedItem a) = toJSON a
  toJSON (SyncedItem a) = toJSON a
  toJSON (UndeletedItem a) = toJSON a

-- | A client-side store of items with Id's of type @i@ and values of type @a@
newtype Store i a =
  Store
    { storeItems :: Set (StoreItem i a)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (Store i a) where
  validate Store {..} =
    mconcat
      [ annotate storeItems "storeItems"
      , declare "the store items have distinct uuids" $
        distinct $
        flip mapMaybe (S.toList storeItems) $ \case
          UnsyncedItem _ -> Nothing
          SyncedItem Synced {..} -> Just syncedUuid
          UndeletedItem u -> Just u
      ]

-- | The store with no items.
emptyStore :: Store i a
emptyStore = Store S.empty

-- | The number of items in a store
storeSize :: Store i a -> Int
storeSize (Store s) =
  S.size $
  S.filter
    (\si ->
       case si of
         UndeletedItem _ -> False
         _ -> True)
    s

-- | Add a new (unsynced) item to the store
addItemToStore :: (Ord i, Ord a) => Added a -> Store i a -> Store i a
addItemToStore a (Store s) = Store $ S.insert (UnsyncedItem a) s

deleteUnsynced :: (Ord i, Ord a) => Added a -> Store i a -> Store i a
deleteUnsynced a (Store s) =
  Store $
  flip mapSetMaybe s $ \si ->
    case si of
      UnsyncedItem i ->
        if i == a
          then Nothing
          else Just si
      SyncedItem _ -> Just si
      UndeletedItem _ -> Just si

deleteSynced :: (Ord i, Ord a) => Synced i a -> Store i a -> Store i a
deleteSynced a (Store s) =
  Store $
  flip mapSetMaybe s $ \si ->
    case si of
      UnsyncedItem _ -> Just si
      SyncedItem i ->
        if i == a
          then Just $ UndeletedItem $ syncedUuid i
          else Just si
      UndeletedItem _ -> Just si

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data SyncRequest i a =
  SyncRequest
    { syncRequestAddedItems :: !(Set (Added a))
    , syncRequestSyncedItems :: !(Set i)
    , syncRequestUndeletedItems :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncRequest i a) where
  validate SyncRequest {..} =
    mconcat
      [ annotate syncRequestAddedItems "syncRequestAddedItems"
      , annotate syncRequestSyncedItems "syncRequestSyncedItems"
      , annotate syncRequestUndeletedItems "syncRequestUndeletedItems"
      , declare "the sync request items have distinct ids" $
        distinct $ S.toList syncRequestSyncedItems ++ S.toList syncRequestUndeletedItems
      ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) => FromJSON (SyncRequest i a) where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .: "unsynced" <*> o .: "synced" <*> o .: "undeleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncRequest i a) where
  toJSON SyncRequest {..} =
    object
      [ "unsynced" .= syncRequestAddedItems
      , "synced" .= syncRequestSyncedItems
      , "undeleted" .= syncRequestUndeletedItems
      ]

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse i a =
  SyncResponse
    { syncResponseAddedItems :: !(Set (Synced i a))
    , syncResponseNewRemoteItems :: !(Set (Synced i a))
    , syncResponseItemsToBeDeletedLocally :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncResponse i a) where
  validate SyncResponse {..} =
    mconcat
      [ annotate syncResponseAddedItems "syncResponseAddedItems"
      , annotate syncResponseNewRemoteItems "syncResponseNewRemoteItems"
      , annotate syncResponseItemsToBeDeletedLocally "syncResponseItemsToBeDeletedLocally"
      , declare "the sync response items have distinct uuids" $
        distinct $
        map syncedUuid (S.toList syncResponseAddedItems ++ S.toList syncResponseNewRemoteItems) ++
        S.toList syncResponseItemsToBeDeletedLocally
      ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) => FromJSON (SyncResponse i a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "added" <*> o .: "new" <*> o .: "deleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncResponse i a) where
  toJSON SyncResponse {..} =
    object
      [ "added" .= syncResponseAddedItems
      , "new" .= syncResponseNewRemoteItems
      , "deleted" .= syncResponseItemsToBeDeletedLocally
      ]

-- | Produce a synchronisation request for a client-side store.
--
-- This request can then be sent to a central store for synchronisation.
makeSyncRequest :: (Ord i, Ord a) => Store i a -> SyncRequest i a
makeSyncRequest Store {..} =
  SyncRequest
    { syncRequestAddedItems =
        flip mapSetMaybe storeItems $ \case
          UnsyncedItem a -> Just a
          _ -> Nothing
    , syncRequestSyncedItems =
        flip mapSetMaybe storeItems $ \case
          SyncedItem i -> Just $ syncedUuid i
          _ -> Nothing
    , syncRequestUndeletedItems =
        flip mapSetMaybe storeItems $ \case
          UndeletedItem uuid -> Just uuid
          _ -> Nothing
    }

-- | Merge a synchronisation response back into a client-side store.
mergeSyncResponse :: (Ord i, Ord a) => Store i a -> SyncResponse i a -> Store i a
mergeSyncResponse s SyncResponse {..} =
  let withNewOwnItems =
        flip mapSetMaybe (storeItems s) $ \si ->
          case si of
            UnsyncedItem Added {..} ->
              case find
                     (\Synced {..} -> syncedCreated == addedCreated && syncedValue == addedValue)
                     syncResponseAddedItems of
                Nothing -> Just si -- If it wasn't added (for whatever reason), just leave it as unsynced
                Just ii -> Just $ SyncedItem ii -- If it was added, then it becomes synced
            SyncedItem ii ->
              case find (== syncedUuid ii) syncResponseItemsToBeDeletedLocally of
                Nothing -> Just si -- If it wasn't deleted, don't delete it.
                Just _ -> Nothing -- If it was deleted, delete it here.
            UndeletedItem _ -> Nothing -- Delete all locally deleted items after sync
   in Store
        { storeItems =
            S.fromList .
            nubBy
              (\i1 i2 ->
                 case (i1, i2) of
                   (UnsyncedItem _, _) -> False
                   (_, UnsyncedItem _) -> False
                   (SyncedItem s1, SyncedItem s2) -> syncedUuid s1 == syncedUuid s2
                   (SyncedItem s1, UndeletedItem u2) -> syncedUuid s1 == u2
                   (UndeletedItem u1, SyncedItem s2) -> u1 == syncedUuid s2
                   (UndeletedItem u1, UndeletedItem u2) -> u1 == u2) .
            S.toList $
            S.map SyncedItem syncResponseNewRemoteItems `S.union` withNewOwnItems
        }

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data SyncProcessor i a m =
  SyncProcessor
    { syncProcessorDeleteMany :: Set i -> m () -- ^ Delete the items with an identifier in the given set.
    , syncProcessorQuerySynced :: Set i -> m (Set i) -- ^ Query the identifiers of the items that are in store, of the given set.
    , syncProcessorQueryNewRemote :: Set i -> m (Set (Synced i a)) -- ^ Query the items that are in store, but not in the given set.
    , syncProcessorInsertMany :: Set (CentralItem a) -> m (Set (Synced i a)) -- ^ Insert a set of items into the store.
    }
  deriving (Generic)

-- | Process a server-side synchronisation request using a custom synchronisation processor
--
-- WARNING: The identifier generation function must produce newly unique identifiers such that each new item gets a unique identifier.
--
-- You can use this function with deterministically-random identifiers or incrementing identifiers.
processSyncCustom ::
     forall i a m. (Ord i, Ord a, Monad m)
  => UTCTime
  -> SyncProcessor i a m
  -> SyncRequest i a
  -> m (SyncResponse i a)
processSyncCustom now SyncProcessor {..} SyncRequest {..} = do
  deleteUndeleted
        -- First we delete the items that were deleted locally but not yet remotely.
        -- Then we find the items that have been deleted remotely but not locally
  deletedRemotely <- syncItemsToBeDeletedLocally
        -- Then we find the items that have appeared remotely but aren't known locally
  newRemoteItems <- syncNewRemoteItems
        -- Then we add the items that should be added.
  newLocalItems <- syncAddedItems
  pure
    SyncResponse
      { syncResponseNewRemoteItems = newRemoteItems
      , syncResponseAddedItems = newLocalItems
      , syncResponseItemsToBeDeletedLocally = deletedRemotely
      }
  where
    deleteUndeleted :: m ()
    deleteUndeleted = syncProcessorDeleteMany syncRequestUndeletedItems
    syncItemsToBeDeletedLocally :: m (Set i)
    syncItemsToBeDeletedLocally = do
      foundItems <- syncProcessorQuerySynced syncRequestSyncedItems
      pure $ syncRequestSyncedItems `S.difference` foundItems
    syncNewRemoteItems :: m (Set (Synced i a))
    syncNewRemoteItems = syncProcessorQueryNewRemote syncRequestSyncedItems
    syncAddedItems :: m (Set (Synced i a))
    syncAddedItems =
      syncProcessorInsertMany $
      flip S.map syncRequestAddedItems $ \Added {..} ->
        CentralItem {centralValue = addedValue, centralSynced = now, centralCreated = addedCreated}

-- | An item in a central store with a value of type @a@
data CentralItem a =
  CentralItem
    { centralValue :: !a
    , centralSynced :: !UTCTime
    , centralCreated :: !UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (CentralItem a)

instance FromJSON a => FromJSON (CentralItem a)

instance ToJSON a => ToJSON (CentralItem a)

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype CentralStore i a =
  CentralStore
    { centralStoreItems :: Map i (CentralItem a)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (CentralStore i a)

-- | Process a server-side synchronisation request using @getCurrentTime@
--
-- see 'processSyncCustom'
processSync ::
     (Ord i, Ord a, MonadIO m)
  => m i
  -> CentralStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, CentralStore i a)
processSync genId cs sr = do
  now <- liftIO getCurrentTime
  processSyncWith genId now cs sr

-- | Process a server-side synchronisation request using a time of syncing, and an identifier generation function.
--
-- see 'processSyncCustom'
processSyncWith ::
     forall i a m. (Ord i, Ord a, Monad m)
  => m i
  -> UTCTime
  -> CentralStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, CentralStore i a)
processSyncWith genUuid now cs sr =
  flip runStateT cs $
  processSyncCustom
    now
    SyncProcessor
      { syncProcessorDeleteMany = deleteMany
      , syncProcessorQuerySynced = querySynced
      , syncProcessorQueryNewRemote = queryNewRemote
      , syncProcessorInsertMany = insertMany
      }
    sr
  where
    deleteMany :: Set i -> StateT (CentralStore i a) m ()
    deleteMany s = modC (`M.withoutKeys` s)
    querySynced :: Set i -> StateT (CentralStore i a) m (Set i)
    querySynced s = M.keysSet <$> query (`M.restrictKeys` s)
    queryNewRemote :: Set i -> StateT (CentralStore i a) m (Set (Synced i a))
    queryNewRemote s = do
      m <- query (`M.withoutKeys` s)
      pure $
        S.fromList $
        flip map (M.toList m) $ \(i, CentralItem {..}) ->
          Synced
            { syncedUuid = i
            , syncedCreated = centralCreated
            , syncedSynced = centralSynced
            , syncedValue = centralValue
            }
    query :: (Map i (CentralItem a) -> b) -> StateT (CentralStore i a) m b
    query func = gets $ func . centralStoreItems
    insertMany :: Set (CentralItem a) -> StateT (CentralStore i a) m (Set (Synced i a))
    insertMany s =
      fmap S.fromList $
      forM (S.toList s) $ \ci@CentralItem {..} -> do
        i <- lift genUuid
        ins i ci
        let si =
              Synced
                { syncedUuid = i
                , syncedCreated = centralCreated
                , syncedSynced = centralSynced
                , syncedValue = centralValue
                }
        pure si
    ins :: i -> CentralItem a -> StateT (CentralStore i a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map i (CentralItem a) -> Map i (CentralItem a)) -> StateT (CentralStore i a) m ()
    modC func = modify (\(CentralStore m) -> CentralStore $ func m)

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
