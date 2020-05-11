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
  ( ClientId (..),
    ClientStore (..),
    emptyClientStore,
    storeSize,
    addItemToClientStore,
    deleteUnsyncedFromClientStore,
    deleteSyncedFromClientStore,
    SyncRequest (..),
    SyncResponse (..),
    emptySyncResponse,

    -- * Client-side Synchronisation
    makeSyncRequest,
    mergeSyncResponse,
    pureClientSyncProcessor,
    ClientSyncProcessor (..),
    mergeSyncResponseCustom,

    -- * Server-side Synchronisation

    -- ** General synchronisation
    ServerSyncProcessor (..),
    processServerSyncCustom,

    -- ** Synchronisation with a simple central store
    ServerStore (..),
    emptyServerStore,
    processServerSync,
  )
where

import Control.DeepSeq
import Control.Monad.State.Strict
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Validity
import Data.Validity.Containers ()
import Data.Word
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

-- | A Client-side identifier for items.
--
-- These only need to be unique at the client.
newtype ClientId
  = ClientId
      { unClientId :: Word64
      }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

instance NFData ClientId

-- | A client-side store of items with Id's of type @i@ and values of type @a@
data ClientStore i a
  = ClientStore
      { clientStoreAdded :: !(Map ClientId a),
        clientStoreSynced :: !(Map i a),
        clientStoreDeleted :: !(Set i)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (ClientStore i a)

instance (Validity i, Validity a, Show i, Show a, Ord i, Ord a) => Validity (ClientStore i a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs,
        declare "the store items have distinct ids"
          $ distinct
          $ M.keys clientStoreSynced ++ S.toList clientStoreDeleted
      ]

instance (Ord i, FromJSON i, FromJSONKey i, FromJSON a) => FromJSON (ClientStore i a) where
  parseJSON =
    withObject "ClientStore" $ \o ->
      ClientStore <$> o .:? "added" .!= M.empty <*> o .:? "synced" .!= M.empty
        <*> o .:? "deleted" .!= S.empty

instance (Ord i, ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (ClientStore i a) where
  toJSON ClientStore {..} =
    object
      ["added" .= clientStoreAdded, "synced" .= clientStoreSynced, "deleted" .= clientStoreDeleted]

-- | The store with no items.
emptyClientStore :: ClientStore i a
emptyClientStore =
  ClientStore
    { clientStoreAdded = M.empty,
      clientStoreSynced = M.empty,
      clientStoreDeleted = S.empty
    }

-- | The number of items in a store
--
-- This does not count the deleted items, so that those really look deleted.
storeSize :: ClientStore i a -> Int
storeSize ClientStore {..} = M.size clientStoreAdded + M.size clientStoreSynced

clientStoreIds :: Ord i => ClientStore i a -> Set i
clientStoreIds ClientStore {..} = M.keysSet clientStoreSynced `S.union` clientStoreDeleted

-- | Add an item to a client store as an added item.
--
-- This will take care of the uniqueness constraint of the 'ClientId's in the map.
addItemToClientStore :: (Ord i, Ord a) => a -> ClientStore i a -> ClientStore i a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAdded cs
      newAddedItems =
        let newKey = findFreeSpot oldAddedItems
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAdded = newAddedItems}

-- | Find a free client id to use
--
-- You shouldn't need this function, 'addItemToClientStore' takes care of this.
findFreeSpot :: Map ClientId a -> ClientId
findFreeSpot m =
  if M.null m
    then ClientId 0
    else
      let (i, _) = M.findMax m
       in go (next i)
  where
    go i =
      if M.member i m
        then go (next i)
        else i
    next (ClientId w)
      | w == maxBound = ClientId 0
      | otherwise = ClientId $ succ w

deleteUnsyncedFromClientStore :: (Ord i, Ord a) => ClientId -> ClientStore i a -> ClientStore i a
deleteUnsyncedFromClientStore cid cs = cs {clientStoreAdded = M.delete cid $ clientStoreAdded cs}

deleteSyncedFromClientStore :: (Ord i, Ord a) => i -> ClientStore i a -> ClientStore i a
deleteSyncedFromClientStore i cs =
  let syncedBefore = clientStoreSynced cs
   in case M.lookup i syncedBefore of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSynced = M.delete i syncedBefore,
              clientStoreDeleted = S.insert i $ clientStoreDeleted cs
            }

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data SyncRequest i a
  = SyncRequest
      { syncRequestAdded :: !(Map ClientId a),
        syncRequestSynced :: !(Set i),
        syncRequestDeleted :: !(Set i)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (SyncRequest i a)

instance (Validity i, Validity a, Ord i, Ord a) => Validity (SyncRequest i a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr,
        declare "the sync request items have distinct ids"
          $ distinct
          $ S.toList syncRequestSynced ++ S.toList syncRequestDeleted
      ]

instance (FromJSON i, FromJSON a, Ord i, Ord a) => FromJSON (SyncRequest i a) where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .: "added" <*> o .: "synced" <*> o .: "undeleted"

instance (ToJSON i, ToJSON a) => ToJSON (SyncRequest i a) where
  toJSON SyncRequest {..} =
    object
      [ "added" .= syncRequestAdded,
        "synced" .= syncRequestSynced,
        "undeleted" .= syncRequestDeleted
      ]

-- | Produce a synchronisation request for a client-side store.
--
-- This request can then be sent to a central store for synchronisation.
makeSyncRequest :: (Ord i, Ord a) => ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestAdded = clientStoreAdded,
      syncRequestSynced = M.keysSet clientStoreSynced,
      syncRequestDeleted = clientStoreDeleted
    }

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse i a
  = SyncResponse
      { syncResponseClientAdded :: !(Map ClientId i),
        syncResponseClientDeleted :: !(Set i),
        syncResponseServerAdded :: !(Map i a),
        syncResponseServerDeleted :: !(Set i)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData i, NFData a) => NFData (SyncResponse i a)

instance (Validity i, Validity a, Show i, Show a, Ord i, Ord a) => Validity (SyncResponse i a) where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr,
        declare "the sync response items have distinct uuids"
          $ distinct
          $ concat
            [ M.elems syncResponseClientAdded,
              S.toList syncResponseClientDeleted,
              M.keys syncResponseServerAdded,
              S.toList syncResponseServerDeleted
            ]
      ]

instance (Ord i, FromJSON i, FromJSONKey i, Ord a, FromJSON a) => FromJSON (SyncResponse i a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "client-added" <*> o .: "client-deleted" <*> o .: "server-added"
        <*> o
        .: "server-deleted"

instance (ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (SyncResponse i a) where
  toJSON SyncResponse {..} =
    object
      [ "client-added" .= syncResponseClientAdded,
        "client-deleted" .= syncResponseClientDeleted,
        "server-added" .= syncResponseServerAdded,
        "server-deleted" .= syncResponseServerDeleted
      ]

emptySyncResponse :: SyncResponse i ia
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty,
      syncResponseClientDeleted = S.empty,
      syncResponseServerAdded = M.empty,
      syncResponseServerDeleted = S.empty
    }

-- | Merge a synchronisation response back into a client-side store.
mergeSyncResponse ::
  forall i a.
  (Ord i, Ord a) =>
  ClientStore i a ->
  SyncResponse i a ->
  ClientStore i a
mergeSyncResponse s sr =
  flip execState s $
    mergeSyncResponseCustom
      pureClientSyncProcessor
      sr

pureClientSyncProcessor :: forall i a. Ord i => ClientSyncProcessor i a (State (ClientStore i a))
pureClientSyncProcessor =
  ClientSyncProcessor
    { clientSyncProcessorSyncServerAdded = \m -> modify $ \cs ->
        cs {clientStoreSynced = M.union (clientStoreSynced cs) (m `diffSet` clientStoreIds cs)},
      clientSyncProcessorSyncClientAdded = \addedItems -> modify $ \cs ->
        let oldAdded = clientStoreAdded cs
            oldSynced = clientStoreSynced cs
            go :: (Map ClientId a, Map i a) -> ClientId -> i -> (Map ClientId a, Map i a)
            go (added, synced) cid i =
              case M.lookup cid added of
                Nothing -> (added, synced)
                Just a -> (M.delete cid added, M.insert i a synced)
            (newAdded, newSynced) = M.foldlWithKey go (oldAdded, oldSynced) addedItems
         in cs {clientStoreAdded = newAdded, clientStoreSynced = newSynced},
      clientSyncProcessorSyncServerDeleted = \toBeDeletedLocally -> modify $ \cs ->
        cs {clientStoreSynced = clientStoreSynced cs `diffSet` toBeDeletedLocally},
      clientSyncProcessorSyncClientDeleted = \cd -> modify $ \cs ->
        cs {clientStoreDeleted = clientStoreDeleted cs `S.difference` cd}
    }

data ClientSyncProcessor i a m
  = ClientSyncProcessor
      { clientSyncProcessorSyncServerAdded :: Map i a -> m (),
        clientSyncProcessorSyncClientAdded :: Map ClientId i -> m (),
        clientSyncProcessorSyncServerDeleted :: Set i -> m (),
        clientSyncProcessorSyncClientDeleted :: Set i -> m ()
      }
  deriving (Generic)

mergeSyncResponseCustom :: Applicative m => ClientSyncProcessor i a m -> SyncResponse i a -> m ()
mergeSyncResponseCustom ClientSyncProcessor {..} SyncResponse {..} =
  clientSyncProcessorSyncServerAdded syncResponseServerAdded
    <* clientSyncProcessorSyncClientAdded syncResponseClientAdded
    <* clientSyncProcessorSyncServerDeleted syncResponseServerDeleted
    <* clientSyncProcessorSyncClientDeleted syncResponseClientDeleted

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data ServerSyncProcessor i a m
  = ServerSyncProcessor
      { -- | Delete the items with an identifier in the given set, return the set that was indeed deleted or did not exist.
        -- In particular, return the identifiers of the items that the client should forget about.
        serverSyncProcessorDeleteMany :: Set i -> m (Set i),
        -- | Query the identifiers of the items that are in the given set but not in the store.
        serverSyncProcessorQueryNoLongerSynced :: Set i -> m (Set i),
        -- | Query the items that are in store, but not in the given set.
        serverSyncProcessorQueryNewRemote :: Set i -> m (Map i a),
        -- | Insert a set of items into the store.
        serverSyncProcessorInsertMany :: Map ClientId a -> m (Map ClientId i)
      }
  deriving (Generic)

-- | Process a server-side synchronisation request using a custom synchronisation processor
--
-- WARNING: The identifier generation function must produce newly unique identifiers such that each new item gets a unique identifier.
--
-- You can use this function with deterministically-random identifiers or incrementing identifiers.
processServerSyncCustom ::
  forall i a m.
  (Ord i, Ord a, Monad m) =>
  ServerSyncProcessor i a m ->
  SyncRequest i a ->
  m (SyncResponse i a)
processServerSyncCustom ServerSyncProcessor {..} SyncRequest {..} = do
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
      { syncResponseClientAdded = newLocalItems,
        syncResponseClientDeleted = deletedFromClient,
        syncResponseServerAdded = newRemoteItems,
        syncResponseServerDeleted = deletedRemotely
      }
  where
    deleteUndeleted :: m (Set i)
    deleteUndeleted = serverSyncProcessorDeleteMany syncRequestDeleted
    syncItemsToBeDeletedLocally :: m (Set i)
    syncItemsToBeDeletedLocally = serverSyncProcessorQueryNoLongerSynced syncRequestSynced
    syncNewRemoteItems :: m (Map i a)
    syncNewRemoteItems = serverSyncProcessorQueryNewRemote syncRequestSynced
    syncAddedItems :: m (Map ClientId i)
    syncAddedItems = serverSyncProcessorInsertMany syncRequestAdded

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype ServerStore i a
  = ServerStore
      { serverStoreItems :: Map i a
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
  forall m i a.
  (Ord i, Ord a, Monad m) =>
  m i ->
  ServerStore i a ->
  SyncRequest i a ->
  m (SyncResponse i a, ServerStore i a)
processServerSync genUuid cs sr =
  flip runStateT cs $
    processServerSyncCustom
      ServerSyncProcessor
        { serverSyncProcessorDeleteMany = deleteMany,
          serverSyncProcessorQueryNoLongerSynced = queryNoLongerSynced,
          serverSyncProcessorQueryNewRemote = queryNewRemote,
          serverSyncProcessorInsertMany = insertMany
        }
      sr
  where
    deleteMany :: Set i -> StateT (ServerStore i a) m (Set i)
    deleteMany s = do
      modC (`diffSet` s)
      pure s
    queryNoLongerSynced :: Set i -> StateT (ServerStore i a) m (Set i)
    queryNoLongerSynced s = query ((s `S.difference`) . M.keysSet)
    queryNewRemote :: Set i -> StateT (ServerStore i a) m (Map i a)
    queryNewRemote s = query (`diffSet` s)
    query :: (Map i a -> b) -> StateT (ServerStore i a) m b
    query func = gets $ func . serverStoreItems
    insertMany :: Map ClientId a -> StateT (ServerStore i a) m (Map ClientId i)
    insertMany =
      traverse $ \a -> do
        u <- lift genUuid
        ins u a
        pure u
    ins :: i -> a -> StateT (ServerStore i a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map i a -> Map i a) -> StateT (ServerStore i a) m ()
    modC func = modify (\(ServerStore m) -> ServerStore $ func m)

diffSet :: Ord i => Map i a -> Set i -> Map i a
diffSet m s = m `M.difference` toMap s

toMap :: Set i -> Map i ()
toMap = M.fromSet (const ())

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
