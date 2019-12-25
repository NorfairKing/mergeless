{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , emptySyncResponse
  , ClientAddition(..)
    -- * Client-side Synchronisation
  , makeSyncRequest
  , mergeSyncResponse
  , addRemotelyAddedItems
  , addAddedItems
  , deleteItemsToBeDeletedLocally
  , deleteLocalUndeletedItems
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
import Control.DeepSeq
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

instance NFData ClientId

-- | A client-side store of items with Id's of type @i@ and values of type @a@
data ClientStore i a =
  ClientStore
    { clientStoreAdded :: !(Map ClientId (Added a))
    , clientStoreSynced :: !(Map i (Synced a))
    , clientStoreDeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (ClientStore i a)

instance (Validity i, Validity a, Show i, Show a, Ord i, Ord a) => Validity (ClientStore i a) where
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

clientStoreIds :: Ord i => ClientStore i a -> Set i
clientStoreIds ClientStore {..} = M.keysSet clientStoreSynced `S.union` clientStoreDeleted

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

instance (NFData i, NFData a) => NFData (SyncRequest i a)

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
    { syncResponseClientAdded :: !(Map ClientId (ClientAddition i))
    , syncResponseClientDeleted :: !(Set i)
    , syncResponseServerAdded :: !(Map i (Synced a))
    , syncResponseServerDeleted :: !(Set i)
    }
  deriving (Show, Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (SyncResponse i a)

instance (Validity i, Validity a, Show i, Show a, Ord i, Ord a) => Validity (SyncResponse i a) where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr
      , declare "the sync response items have distinct uuids" $
        distinct $
        concat
          [ M.elems $ M.map clientAdditionId syncResponseClientAdded
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

emptySyncResponse :: SyncResponse i ia
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty
    , syncResponseClientDeleted = S.empty
    , syncResponseServerAdded = M.empty
    , syncResponseServerDeleted = S.empty
    }

data ClientAddition i =
  ClientAddition
    { clientAdditionId :: i
    , clientAdditionTime :: UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity i => Validity (ClientAddition i)

instance NFData i => NFData (ClientAddition i)

instance FromJSON i => FromJSON (ClientAddition i) where
  parseJSON = withObject "ClientAddition" $ \o -> ClientAddition <$> o .: "id" <*> o .: "time"

instance ToJSON i => ToJSON (ClientAddition i) where
  toJSON ClientAddition {..} = object ["id" .= clientAdditionId, "time" .= clientAdditionTime]

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
addRemotelyAddedItems m cs =
  cs {clientStoreSynced = M.union (clientStoreSynced cs) (m `diffSet` clientStoreIds cs)}

addAddedItems ::
     forall i a. (Ord i, Ord a)
  => Map ClientId (ClientAddition i)
  -> ClientStore i a
  -> ClientStore i a
addAddedItems addedItems cs =
  let oldAdded = clientStoreAdded cs
      oldSynced = clientStoreSynced cs
      go ::
           (Map ClientId (Added a), Map i (Synced a))
        -> ClientId
        -> ClientAddition i
        -> (Map ClientId (Added a), Map i (Synced a))
      go (added, synced) cid ClientAddition {..} =
        case M.lookup cid added of
          Nothing -> (added, synced)
          Just a ->
            let s = addedToSynced clientAdditionTime a
             in (M.delete cid added, M.insert clientAdditionId s synced)
      (newAdded, newSynced) = M.foldlWithKey go (oldAdded, oldSynced) addedItems
   in cs {clientStoreAdded = newAdded, clientStoreSynced = newSynced}

deleteItemsToBeDeletedLocally :: (Ord i, Ord a) => Set i -> ClientStore i a -> ClientStore i a
deleteItemsToBeDeletedLocally toBeDeletedLocally cs =
  cs {clientStoreSynced = clientStoreSynced cs `diffSet` toBeDeletedLocally}

deleteLocalUndeletedItems :: (Ord i, Ord a) => Set i -> ClientStore i a -> ClientStore i a
deleteLocalUndeletedItems cd cs = cs {clientStoreDeleted = clientStoreDeleted cs `S.difference` cd}

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data ServerSyncProcessor i a m =
  ServerSyncProcessor
    { serverSyncProcessorDeleteMany :: Set i -> m (Set i)
      -- ^ Delete the items with an identifier in the given set, return the set that was indeed deleted or did not exist.
      -- In particular, return the identifiers of the items that the client should forget about.
    , serverSyncProcessorQueryNoLongerSynced :: Set i -> m (Set i) -- ^ Query the identifiers of the items that are in the given set but not in the store.
    , serverSyncProcessorQueryNewRemote :: Set i -> m (Map i (Synced a)) -- ^ Query the items that are in store, but not in the given set.
    , serverSyncProcessorInsertMany :: Map ClientId (Added a) -> m (Map ClientId (ClientAddition i)) -- ^ Insert a set of items into the store.
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
    syncItemsToBeDeletedLocally = serverSyncProcessorQueryNoLongerSynced syncRequestSynced
    syncNewRemoteItems :: m (Map i (Synced a))
    syncNewRemoteItems = serverSyncProcessorQueryNewRemote syncRequestSynced
    syncAddedItems :: m (Map ClientId (ClientAddition i))
    syncAddedItems = serverSyncProcessorInsertMany syncRequestAdded

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (Synced a)
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (NFData i, NFData a) => NFData (ServerStore i a)

instance (Validity i, Validity a, Show i, Show a, Ord i, Ord a) => Validity (ServerStore i a)

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
      , serverSyncProcessorQueryNoLongerSynced = queryNoLongerSynced
      , serverSyncProcessorQueryNewRemote = queryNewRemote
      , serverSyncProcessorInsertMany = insertMany
      }
    sr
  where
    deleteMany :: Set i -> StateT (ServerStore i a) m (Set i)
    deleteMany s = do
      modC (`diffSet` s)
      pure s
    queryNoLongerSynced :: Set i -> StateT (ServerStore i a) m (Set i)
    queryNoLongerSynced s = query ((s `S.difference`) . M.keysSet)
    queryNewRemote :: Set i -> StateT (ServerStore i a) m (Map i (Synced a))
    queryNewRemote s = query (`diffSet` s)
    query :: (Map i (Synced a) -> b) -> StateT (ServerStore i a) m b
    query func = gets $ func . serverStoreItems
    insertMany ::
         Map ClientId (Added a) -> StateT (ServerStore i a) m (Map ClientId (ClientAddition i))
    insertMany =
      M.traverseWithKey $ \cid a -> do
        u <- lift genUuid
        let s = addedToSynced now a
        ins u s
        pure ClientAddition {clientAdditionId = u, clientAdditionTime = now}
    ins :: i -> Synced a -> StateT (ServerStore i a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map i (Synced a) -> Map i (Synced a)) -> StateT (ServerStore i a) m ()
    modC func = modify (\(ServerStore m) -> ServerStore $ func m)

diffSet :: Ord i => Map i a -> Set i -> Map i a
diffSet m s = m `M.difference` toMap s

toMap :: Set i -> Map i ()
toMap = M.fromSet (const ())

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
