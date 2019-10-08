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
  ( ClientId(..)
  , Added(..)
  , Synced(..)
  , ClientStore(..)
  , emptyClientStore
  , storeSize
  , addItemToClientStore
  , deleteUnsyncedFromClientStore
  , deleteSyncedFromClientStore
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
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (ClientStore i a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs
      , declare "the store items have distinct ids" $
        distinct $ M.keys clientStoreSynced ++ S.toList clientStoreDeleted
      ]

instance (Ord i, FromJSON i, FromJSONKey i, FromJSON a) => FromJSON (ClientStore i a)

instance (Ord i, ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (ClientStore i a)

-- | The store with no items.
emptyClientStore :: ClientStore i a
emptyClientStore =
  ClientStore
    {clientStoreAdded = M.empty, clientStoreSynced = M.empty, clientStoreDeleted = S.empty}

-- | The number of items in a store
--
-- This does not count the deleted items, so that those really look deleted.
storeSize :: ClientStore i a -> Int
storeSize ClientStore {..} = M.size clientStoreAdded + M.size clientStoreSynced

-- | Add a new (unsynced) item to the store
addItemToClientStore :: (Ord i, Ord a) => Added a -> ClientStore i a -> ClientStore i a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAdded cs
      newAddedItems =
        let newKey =
              ClientId $
              if M.null oldAddedItems
                then 0
                else let (ClientId k, _) = M.findMax oldAddedItems
                      in succ k
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAdded = newAddedItems}

deleteUnsyncedFromClientStore :: (Ord i, Ord a) => ClientId -> ClientStore i a -> ClientStore i a
deleteUnsyncedFromClientStore cid cs = cs {clientStoreAdded = M.delete cid $ clientStoreAdded cs}

deleteSyncedFromClientStore :: (Ord i, Ord a) => i -> ClientStore i a -> ClientStore i a
deleteSyncedFromClientStore i cs =
  let syncedBefore = clientStoreSynced cs
   in case M.lookup i syncedBefore of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSynced = M.delete i syncedBefore
            , clientStoreDeleted = S.insert i $ clientStoreDeleted cs
            }

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data SyncRequest i a =
  SyncRequest
    { syncRequestAdded :: !(Map ClientId (Added a))
    , syncRequestSynced :: !(Set i)
    , syncRequestDeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncRequest i a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr
      , declare "the sync request items have distinct ids" $
        distinct $ S.toList syncRequestSynced ++ S.toList syncRequestDeleted
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
      , "undeleted" .= syncRequestDeleted
      ]

-- | Produce a synchronisation request for a client-side store.
--
-- This request can then be sent to a central store for synchronisation.
makeSyncRequest :: (Ord i, Ord a) => ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestAdded = clientStoreAdded
    , syncRequestSynced = M.keysSet clientStoreSynced
    , syncRequestDeleted = clientStoreDeleted
    }

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse i a =
  SyncResponse
    { syncResponseClientAdded :: !(Map ClientId (i, UTCTime))
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
          [ M.elems $ M.map fst syncResponseClientAdded
          , S.toList syncResponseClientDeleted
          , M.keys syncResponseServerAdded
          , S.toList syncResponseServerDeleted
          ]
      ]

instance (Ord i, FromJSON i, FromJSONKey i, Ord a, FromJSON a) => FromJSON (SyncResponse i a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "client-added" <*> o .: "client-deleted" <*> o .: "server-added" <*>
      o .: "server-deleted"

instance (ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (SyncResponse i a) where
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
  addRemotelyAddedItems syncResponseServerAdded .
  addAddedItems syncResponseClientAdded .
  deleteItemsToBeDeletedLocally syncResponseServerDeleted .
  deleteLocalUndeletedItems syncResponseClientDeleted $
  s

addRemotelyAddedItems :: (Ord i, Ord a) => Map i (Synced a) -> ClientStore i a -> ClientStore i a
addRemotelyAddedItems = undefined

addAddedItems :: (Ord i, Ord a) => Map ClientId (i, UTCTime) -> ClientStore i a -> ClientStore i a
addAddedItems addedItems sis = undefined

deleteItemsToBeDeletedLocally :: (Ord i, Ord a) => Set i -> ClientStore i a -> ClientStore i a
deleteItemsToBeDeletedLocally toBeDeletedLocally sis = undefined

deleteLocalUndeletedItems :: (Ord i, Ord a) => Set i -> ClientStore i a -> ClientStore i a
deleteLocalUndeletedItems cd sis = undefined

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data ServerSyncProcessor i a m =
  ServerSyncProcessor
    { serverSyncProcessorDeleteMany :: Set i -> m (Set i) -- ^ Delete the items with an identifier in the given set, return the set that was indeed deleted.
    , serverSyncProcessorQuerySynced :: Set i -> m (Set i) -- ^ Query the identifiers of the items that are in store, of the given set.
    , serverSyncProcessorQueryNewRemote :: Set i -> m (Map i (Synced a)) -- ^ Query the items that are in store, but not in the given set.
    , serverSyncProcessorInsertMany :: Map ClientId (Added a) -> m (Map ClientId (i, UTCTime)) -- ^ Insert a set of items into the store.
    }
  deriving (Generic)

-- | Process a server-side synchronisation request using a custom synchronisation processor
--
-- WARNING: The identifier generation function must produce newly unique identifiers such that each new item gets a unique identifier.
--
-- You can use this function with deterministically-random identifiers or incrementing identifiers.
processServerSyncCustom ::
     forall i a m. (Ord i, Ord a, Monad m)
  => UTCTime
  -> ServerSyncProcessor i a m
  -> SyncRequest i a
  -> m (SyncResponse i a)
processServerSyncCustom now ServerSyncProcessor {..} SyncRequest {..} = do
  deletedFromClient <- deleteUndeleted
        -- First we delete the items that were deleted locally but not yet remotely.
        -- Then we find the items that have been deleted remotely but not locally
  deletedRemotely <- syncItemsToBeDeletedLocally
        -- Then we find the items that have appeared remotely but aren't known locally
  newRemoteItems <- syncNewRemoteItems
        -- Then we add the items that should be added.
  newLocalItems <- syncAddedItems
  pure
    SyncResponse
      { syncResponseClientAdded = newLocalItems
      , syncResponseClientDeleted = deletedFromClient
      , syncResponseServerAdded = newRemoteItems
      , syncResponseServerDeleted = deletedRemotely
      }
  where
    deleteUndeleted :: m (Set i)
    deleteUndeleted = serverSyncProcessorDeleteMany syncRequestDeleted
    syncItemsToBeDeletedLocally :: m (Set i)
    syncItemsToBeDeletedLocally = do
      foundItems <- serverSyncProcessorQuerySynced syncRequestSynced
      pure $ syncRequestSynced `S.difference` foundItems
    syncNewRemoteItems :: m (Map i (Synced a))
    syncNewRemoteItems = serverSyncProcessorQueryNewRemote syncRequestSynced
    syncAddedItems :: m (Map ClientId (i, UTCTime))
    syncAddedItems = serverSyncProcessorInsertMany syncRequestAdded

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (Synced a)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (ServerStore i a)

-- | An empty central store to start with
emptyServerStore :: ServerStore i a
emptyServerStore = ServerStore {serverStoreItems = M.empty}

-- | Process a server-side synchronisation request using @getCurrentTime@
--
-- see 'processSyncCustom'
processServerSync ::
     (Ord i, Ord a, MonadIO m)
  => m i
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
processServerSync genId cs sr = do
  now <- liftIO getCurrentTime
  processServerSyncWith genId now cs sr

-- | Process a server-side synchronisation request using a time of syncing, and an identifier generation function.
--
-- see 'processSyncCustom'
processServerSyncWith ::
     forall i a m. (Ord i, Ord a, Monad m)
  => m i
  -> UTCTime
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
processServerSyncWith genUuid now cs sr =
  flip runStateT cs $
  processServerSyncCustom
    now
    ServerSyncProcessor
      { serverSyncProcessorDeleteMany = deleteMany
      , serverSyncProcessorQuerySynced = querySynced
      , serverSyncProcessorQueryNewRemote = queryNewRemote
      , serverSyncProcessorInsertMany = insertMany
      }
    sr
  where
    deleteMany :: Set i -> StateT (ServerStore i a) m (Set i)
    deleteMany s = undefined
    querySynced :: Set i -> StateT (ServerStore i a) m (Set i)
    querySynced s = M.keysSet <$> query (`M.restrictKeys` s)
    queryNewRemote :: Set i -> StateT (ServerStore i a) m (Map i (Synced a))
    queryNewRemote s = undefined
    query :: (Map i (Synced a) -> b) -> StateT (ServerStore i a) m b
    query func = gets $ func . serverStoreItems
    insertMany :: Map ClientId (Added a) -> StateT (ServerStore i a) m (Map ClientId (i, UTCTime))
    insertMany s = undefined
    ins :: i -> Synced a -> StateT (ServerStore i a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map i (Synced a) -> Map i (Synced a)) -> StateT (ServerStore i a) m ()
    modC func = modify (\(ServerStore m) -> ServerStore $ func m)

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
