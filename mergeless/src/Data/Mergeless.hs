{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeless
    ( Store(..)
    , StoreItem(..)
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , makeSyncRequest
    , SyncResponse(..)
    , mergeSyncResponse
    , CentralStore(..)
    , CentralItem(..)
    , processSync
    , processSyncWith
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

newtype Store i a = Store
    { storeItems :: Set (StoreItem i a)
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

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

data StoreItem i a
    = UnsyncedItem !(Added a)
    | SyncedItem !(Synced i a)
    | UndeletedItem i
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

data Added a = Added
    { addedValue :: !a
    , addedCreated :: !UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Added a)

instance FromJSON a => FromJSON (Added a) where
    parseJSON =
        withObject "Added" $ \o -> Added <$> o .: "value" <*> o .: "added"

instance ToJSON a => ToJSON (Added a) where
    toJSON Added {..} = object ["value" .= addedValue, "added" .= addedCreated]

data Synced i a = Synced
    { syncedUuid :: i
    , syncedValue :: !a
    , syncedCreated :: !UTCTime
    , syncedSynced :: !UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a) => Validity (Synced i a)

instance (FromJSON i, FromJSON a) => FromJSON (Synced i a) where
    parseJSON =
        withObject "Synced" $ \o ->
            Synced <$> o .: "id" <*> o .: "value" <*> o .: "created" <*>
            o .: "synced"

instance (ToJSON i, ToJSON a) => ToJSON (Synced i a) where
    toJSON Synced {..} =
        object
            [ "id" .= syncedUuid
            , "value" .= syncedValue
            , "created" .= syncedCreated
            , "synced" .= syncedSynced
            ]

data SyncRequest i a = SyncRequest
    { syncRequestAddedItems :: !(Set (Added a))
    , syncRequestSyncedItems :: !(Set i)
    , syncRequestUndeletedItems :: !(Set i)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) =>
         Validity (SyncRequest i a) where
    validate SyncRequest {..} =
        mconcat
            [ annotate syncRequestAddedItems "syncRequestAddedItems"
            , annotate syncRequestSyncedItems "syncRequestSyncedItems"
            , annotate syncRequestUndeletedItems "syncRequestUndeletedItems"
            , declare "the sync request items have distinct ids" $
              distinct $
              S.toList syncRequestSyncedItems ++
              S.toList syncRequestUndeletedItems
            ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) =>
         FromJSON (SyncRequest i a) where
    parseJSON =
        withObject "SyncRequest" $ \o ->
            SyncRequest <$> o .: "unsynced" <*> o .: "synced" <*>
            o .: "undeleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncRequest i a) where
    toJSON SyncRequest {..} =
        object
            [ "unsynced" .= syncRequestAddedItems
            , "synced" .= syncRequestSyncedItems
            , "undeleted" .= syncRequestUndeletedItems
            ]

data SyncResponse i a = SyncResponse
    { syncResponseAddedItems :: !(Set (Synced i a))
    , syncResponseNewRemoteItems :: !(Set (Synced i a))
    , syncResponseItemsToBeDeletedLocally :: !(Set i)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) =>
         Validity (SyncResponse i a) where
    validate SyncResponse {..} =
        mconcat
            [ annotate syncResponseAddedItems "syncResponseAddedItems"
            , annotate syncResponseNewRemoteItems "syncResponseNewRemoteItems"
            , annotate
                  syncResponseItemsToBeDeletedLocally
                  "syncResponseItemsToBeDeletedLocally"
            , declare "the sync response items have distinct uuids" $
              distinct $
              map
                  syncedUuid
                  (S.toList syncResponseAddedItems ++
                   S.toList syncResponseNewRemoteItems) ++
              S.toList syncResponseItemsToBeDeletedLocally
            ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) =>
         FromJSON (SyncResponse i a) where
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

mergeSyncResponse ::
       (Ord i, Ord a) => Store i a -> SyncResponse i a -> Store i a
mergeSyncResponse s SyncResponse {..} =
    let withNewOwnItems =
            flip mapSetMaybe (storeItems s) $ \si ->
                case si of
                    UnsyncedItem Added {..} ->
                        case find
                                 (\Synced {..} ->
                                      syncedCreated == addedCreated &&
                                      syncedValue == addedValue)
                                 syncResponseAddedItems of
                            Nothing -> Just si -- If it wasn't added (for whatever reason), just leave it as unsynced
                            Just ii -> Just $ SyncedItem ii -- If it was added, then it becomes synced
                    SyncedItem ii ->
                        case find
                                 (== syncedUuid ii)
                                 syncResponseItemsToBeDeletedLocally of
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
                          (SyncedItem s1, SyncedItem s2) ->
                              syncedUuid s1 == syncedUuid s2
                          (SyncedItem s1, UndeletedItem u2) ->
                              syncedUuid s1 == u2
                          (UndeletedItem u1, SyncedItem s2) ->
                              u1 == syncedUuid s2
                          (UndeletedItem u1, UndeletedItem u2) -> u1 == u2) .
             S.toList $
             S.map SyncedItem syncResponseNewRemoteItems `S.union`
             withNewOwnItems
       }

data CentralItem a = CentralItem
    { centralValue :: a
    , centralSynced :: UTCTime
    , centralCreated :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (CentralItem a)

instance FromJSON a => FromJSON (CentralItem a)

instance ToJSON a => ToJSON (CentralItem a)

newtype CentralStore i a = CentralStore
    { centralStoreItems :: Map i (CentralItem a)
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) =>
         Validity (CentralStore i a)

processSync ::
       (Ord i, Ord a, MonadIO m)
    => m i
    -> CentralStore i a
    -> SyncRequest i a
    -> m (SyncResponse i a, CentralStore i a)
processSync genId cs sr = do
    now <- liftIO getCurrentTime
    processSyncWith genId now cs sr

-- | A process a sync request with a custom UUID generation function
--
-- Use this function if you want to process sync requests purely with a pseudorandom generator
processSyncWith ::
       forall i a m. (Ord i, Ord a, Monad m)
    => m i
    -> UTCTime
    -> CentralStore i a
    -> SyncRequest i a
    -> m (SyncResponse i a, CentralStore i a)
processSyncWith genUuid now cs SyncRequest {..} =
    flip runStateT cs $ do
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
    deleteUndeleted :: StateT (CentralStore i a) m ()
    deleteUndeleted = deleteMany syncRequestUndeletedItems
    deleteMany :: Set i -> StateT (CentralStore i a) m ()
    deleteMany s = modC (`M.withoutKeys` s)
    syncItemsToBeDeletedLocally :: StateT (CentralStore i a) m (Set i)
    syncItemsToBeDeletedLocally = do
        foundItems <- query (`M.restrictKeys` syncRequestSyncedItems)
        pure $ syncRequestSyncedItems `S.difference` M.keysSet foundItems
    syncNewRemoteItems :: StateT (CentralStore i a) m (Set (Synced i a))
    syncNewRemoteItems = do
        syncedValues <- query (`M.withoutKeys` syncRequestSyncedItems)
        pure $
            S.fromList $
            flip map (M.toList syncedValues) $ \(u, CentralItem {..}) ->
                Synced
                { syncedUuid = u
                , syncedValue = centralValue
                , syncedCreated = centralCreated
                , syncedSynced = centralSynced
                }
    query :: (Map i (CentralItem a) -> b) -> StateT (CentralStore i a) m b
    query func = gets $ func . centralStoreItems
    syncAddedItems :: StateT (CentralStore i a) m (Set (Synced i a))
    syncAddedItems =
        fmap S.fromList $
        forM (S.toList syncRequestAddedItems) $ \Added {..} -> do
            uuid <- lift genUuid
            ins
                uuid
                CentralItem
                { centralValue = addedValue
                , centralCreated = addedCreated
                , centralSynced = now
                }
            pure
                Synced
                { syncedUuid = uuid
                , syncedCreated = addedCreated
                , syncedSynced = now
                , syncedValue = addedValue
                }
    ins :: i -> CentralItem a -> StateT (CentralStore i a) m ()
    ins i val = modC $ M.insert i val
    modC ::
           (Map i (CentralItem a) -> Map i (CentralItem a))
        -> StateT (CentralStore i a) m ()
    modC func = modify (\(CentralStore m) -> CentralStore $ func m)

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
