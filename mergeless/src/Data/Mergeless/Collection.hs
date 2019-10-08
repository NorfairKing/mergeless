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
-- * The server performs operations according to the functionality of 'processServerSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponse'.
module Data.Mergeless.Collection
  ( Added(..)
  , Synced(..)
  , addedSynced
  , ClientStoreItem(..)
  , ClientStore(..)
  , emptyClientStore
  , storeSize
  , addItemToClientStore
  , deleteUnsynced
  , deleteSynced
  , SyncRequest(..)
  , SyncResponse(..)
    -- * Client-side Synchronisation
  , makeSyncRequest
  , mergeSyncResponse
    -- * Server-side Synchronisation
    -- ** General synchronisation
  , ServerSyncProcessor(..)
  , processServerSyncCustom
    -- ** Synchronisation with a simple central store
  , ServerStore(..)
  , emptyServerStore
  , Synced(..)
  , syncedSynced
  , centralItemSynced
  , processServerSyncWith
  , processServerSync
  ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()

import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import Data.Word

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Mergeless.Item

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

-- | A Client-side identifier for items.
--
-- These only need to be unique at the client.
newtype ClientId =
  ClientId
    { unClientId :: Word64
    }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

-- | A client-side store of items with Id's of type @i@ and values of type @a@
data ClientStore i a =
  ClientStore
    { clientStoreAdded :: !(Map ClientId (Added a))
    , clientStoreSynced :: !(Map i (Synced a))
    , clientStoreDeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (ClientStore i a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs
      , declare "the store items have distinct ids" $
        distinct $ M.keys clientStoreSynced ++ S.toList clientStoreDeleted
      ]

-- | The store with no items.
emptyClientStore :: ClientStore i a
emptyClientStore =
  ClientStore
    {clientStoreAdded = M.empty, clientStoreSynced = M.empty, clientStoreDeleted = M.empty}

-- | The number of items in a store
--
-- This does not count the deleted items, so that those really look deleted.
storeSize :: ClientStore i a -> Int
storeSize ClientStore {..} = M.size clientStoreAdded + M.size clientStoreSynced

-- | Add a new (unsynced) item to the store
addItemToClientStore :: (Ord i, Ord a) => Added a -> ClientStore i a -> ClientStore i a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAddedItems cs
      newAddedItems =
        let newKey =
              ClientId $
              if M.null oldAddedItems
                then 0
                else let (ClientId k, _) = M.findMax oldAddedItems
                      in succ k
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAdded = newAddedItems}

deleteUnsynced :: (Ord i, Ord a) => ClientId -> ClientStore i a -> ClientStore i a
deleteUnsynced cid cs = cs {clientStoreAdded = M.delete cid $ clientStoreAdded cs}

deleteSynced :: (Ord i, Ord a) => i -> ClientStore i a -> ClientStore i a
deleteSynced i cs =
  let syncedBefore = clientStoreSynced cs
   in case M.lookup syncedBefore of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSynced = M.delete i syncedBefore
            , clientStoreDeleted = M.insert i $ clientStoreDeleted cs
            }

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data SyncRequest i a =
  SyncRequest
    { syncRequestAdded :: !(Map ClientId (Added a))
    , syncRequestSynced :: !(Set i)
    , syncRequestUndeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncRequest i a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr
      , declare "the sync request items have distinct ids" $
        distinct $ S.toList syncRequestSynced ++ S.toList syncRequestUndeleted
      ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) => FromJSON (SyncRequest i a) where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .: "added" <*> o .: "synced" <*> o .: "undeleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncRequest i a) where
  toJSON SyncRequest {..} =
    object
      [ "added" .= syncRequestAdded
      , "synced" .= syncRequestSynced
      , "undeleted" .= syncRequestUndeleted
      ]

-- | Produce a synchronisation request for a client-side store.
--
-- This request can then be sent to a central store for synchronisation.
makeSyncRequest :: (Ord i, Ord a) => ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestAdded = clientStoreAdded
    , syncRequestSynced = M.keysSet clientStoreSynced
    , syncRequestUndeleted = clientStoreDeleted
    }

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse i a =
  SyncResponse
    { syncResponseClientAdded :: !(Map i (Synced a))
    , syncResponseClientDeleted :: !(Set i)
    , syncResponseServerAdded :: !(Map i (Synced a))
    , syncResponseServerDeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncResponse i a) where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr
      , declare "the sync response items have distinct uuids" $
        distinct $
        concat
          [ M.keys syncResponseClientAdded
          , S.toList syncResponseClientDeleted
          , M.keys syncResponseServerAdded
          , S.toList syncResponseServerDeleted
          ]
      ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) => FromJSON (SyncResponse i a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "client-added" <*> o .: "client-deleted" <*> o .: "server-added" <*>
      o .: "server-deleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncResponse i a) where
  toJSON SyncResponse {..} =
    object
      [ "client-added" .= syncResponseClientAdded
      , "client-deleted" .= syncResponseClientDeleted
      , "server-added" .= syncResponseServerAdded
      , "server-deleted" .= syncResponseServerDeleted
      ]

-- | Merge a synchronisation response back into a client-side store.
mergeSyncResponse ::
     forall i a. (Ord i, Ord a)
  => ClientStore i a
  -> SyncResponse i a
  -> ClientStore i a
mergeSyncResponse s SyncResponse {..} =
  ClientStore $
  addRemotelyAddedItems syncResponseServerAdded $
  addAddedItems syncResponseClientAdded $
  deleteItemsToBeDeletedLocally syncResponseServerDeleted $
  deleteLocalUndeletedItems syncResponseClientDeleted $ storeItems s

addRemotelyAddedItems ::
     (Ord i, Ord a) => Set (Synced i a) -> Set (ClientStoreItem i a) -> Set (ClientStoreItem i a)
addRemotelyAddedItems remotelyAddedItems sis = S.map SyncedItem remotelyAddedItems `S.union` sis

addAddedItems ::
     (Ord i, Ord a) => Set (Synced i a) -> Set (ClientStoreItem i a) -> Set (ClientStoreItem i a)
addAddedItems addedItems sis =
  flip S.map sis $ \si ->
    case si of
      UnsyncedItem Added {..} ->
        case find
               (\Synced {..} -> syncedCreated == addedCreated && syncedValue == addedValue)
               addedItems of
          Nothing -> si -- If it wasn't added (for whatever reason), just leave it as unsynced
          Just ii -> SyncedItem ii -- If it was added, then it becomes synced
      _ -> si

deleteItemsToBeDeletedLocally ::
     (Ord i, Ord a) => Set i -> Set (ClientStoreItem i a) -> Set (ClientStoreItem i a)
deleteItemsToBeDeletedLocally toBeDeletedLocally sis =
  flip mapSetMaybe sis $ \si ->
    case si of
      SyncedItem ii ->
        case find (== syncedUuid ii) toBeDeletedLocally of
          Nothing -> Just si -- If it wasn't deleted, don't delete it.
          Just _ -> Nothing -- If it was deleted, delete it here.
      _ -> Just si

deleteLocalUndeletedItems ::
     (Ord i, Ord a) => Set i -> Set (ClientStoreItem i a) -> Set (ClientStoreItem i a)
deleteLocalUndeletedItems cd sis =
  flip mapSetMaybe sis $ \si ->
    case si of
      UndeletedItem _ -> Nothing
      _ -> Just si

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data SyncProcessor i a m =
  SyncProcessor
    { syncProcessorDeleteMany :: Set i -> m () -- ^ Delete the items with an identifier in the given set.
    , syncProcessorQuerySynced :: Set i -> m (Set i) -- ^ Query the identifiers of the items that are in store, of the given set.
    , syncProcessorQueryNewRemote :: Set i -> m (Set (Synced i a)) -- ^ Query the items that are in store, but not in the given set.
    , syncProcessorInsertMany :: Set (Synced a) -> m (Set (Synced i a)) -- ^ Insert a set of items into the store.
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
        Synced {centralValue = addedValue, centralSynced = now, centralCreated = addedCreated}

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype ServerStore i a =
  ServerStore
    { centralClientStoreItems :: Map i (Synced a)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (ServerStore i a)

-- | An empty central store to start with
emptyServerStore :: ServerStore i a
emptyServerStore = ServerStore M.empty

-- | Process a server-side synchronisation request using @getCurrentTime@
--
-- see 'processSyncCustom'
processSync ::
     (Ord i, Ord a, MonadIO m)
  => m i
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
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
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
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
    deleteMany :: Set i -> StateT (ServerStore i a) m ()
    deleteMany s = modC (`M.withoutKeys` s)
    querySynced :: Set i -> StateT (ServerStore i a) m (Set i)
    querySynced s = M.keysSet <$> query (`M.restrictKeys` s)
    queryNewRemote :: Set i -> StateT (ServerStore i a) m (Set (Synced i a))
    queryNewRemote s = do
      m <- query (`M.withoutKeys` s)
      pure $ S.fromList $ flip map (M.toList m) $ \(i, ci) -> centralItemSynced i ci
    query :: (Map i (Synced a) -> b) -> StateT (ServerStore i a) m b
    query func = gets $ func . centralClientStoreItems
    insertMany :: Set (Synced a) -> StateT (ServerStore i a) m (Set (Synced i a))
    insertMany s =
      fmap S.fromList $
      forM (S.toList s) $ \ci@Synced {..} -> do
        i <- lift genUuid
        ins i ci
        pure $ centralItemSynced i ci
    ins :: i -> Synced a -> StateT (ServerStore i a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map i (Synced a) -> Map i (Synced a)) -> StateT (ServerStore i a) m ()
    modC func = modify (\(ServerStore m) -> ServerStore $ func m)

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
