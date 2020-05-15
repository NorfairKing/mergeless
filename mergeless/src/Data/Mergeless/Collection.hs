{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- * Items must allow for a client-side unique identifier.
-- * Identifiers for items must be generated in such a way that they are certainly unique.
--
-- Should mutation be a requirement, then there is another library: 'mergeful' for exactly this purpose.
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
  ( ClientStore (..),
    SyncRequest (..),
    SyncResponse (..),

    -- * Client-side Synchronisation

    -- ** General
    ClientSyncProcessor (..),
    mergeSyncResponseCustom,

    -- ** Pure
    emptyClientStore,
    ClientId (..),
    storeSize,
    addItemToClientStore,
    deleteUnsyncedFromClientStore,
    deleteSyncedFromClientStore,
    emptySyncRequest,
    makeSyncRequest,
    mergeSyncResponse,
    pureClientSyncProcessor,

    -- * Server-side Synchronisation

    -- ** General synchronisation
    ServerSyncProcessor (..),
    processServerSyncCustom,

    -- ** Synchronisation with a simple central store
    ServerStore (..),
    emptyServerStore,
    emptySyncResponse,
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

-- | A Client-side identifier for items for use with pure client stores
--
-- These only need to be unique at the client.
newtype ClientId
  = ClientId
      { unClientId :: Word64
      }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

instance NFData ClientId

-- | A client-side store of items with Client Id's of type @ci@, Server Id's of type @i@ and values of type @a@
data ClientStore ci si a
  = ClientStore
      { clientStoreAdded :: !(Map ci a),
        clientStoreSynced :: !(Map si a),
        clientStoreDeleted :: !(Set si)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData ci, NFData si, NFData a) => NFData (ClientStore ci si a)

instance (Validity ci, Validity si, Validity a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => Validity (ClientStore ci si a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs,
        declare "the store items have distinct ids"
          $ distinct
          $ M.keys clientStoreSynced ++ S.toList clientStoreDeleted
      ]

instance (Ord ci, FromJSON ci, FromJSONKey ci, Ord si, FromJSON si, FromJSONKey si, FromJSON a) => FromJSON (ClientStore ci si a) where
  parseJSON =
    withObject "ClientStore" $ \o ->
      ClientStore <$> o .:? "added" .!= M.empty <*> o .:? "synced" .!= M.empty
        <*> o .:? "deleted" .!= S.empty

instance (Ord ci, ToJSON ci, ToJSONKey ci, Ord si, ToJSON si, ToJSONKey si, ToJSON a) => ToJSON (ClientStore ci si a) where
  toJSON ClientStore {..} =
    object
      ["added" .= clientStoreAdded, "synced" .= clientStoreSynced, "deleted" .= clientStoreDeleted]

-- | The client store with no items.
emptyClientStore :: ClientStore ci si a
emptyClientStore =
  ClientStore
    { clientStoreAdded = M.empty,
      clientStoreSynced = M.empty,
      clientStoreDeleted = S.empty
    }

-- | The number of items in a store
--
-- This does not count the deleted items, so that those really look deleted.
storeSize :: ClientStore ci si a -> Int
storeSize ClientStore {..} = M.size clientStoreAdded + M.size clientStoreSynced

clientStoreIds :: Ord si => ClientStore ci si a -> Set si
clientStoreIds ClientStore {..} = M.keysSet clientStoreSynced `S.union` clientStoreDeleted

-- | Add an item to a client store as an added item.
--
-- This will take care of the uniqueness constraint of the 'ci's in the map.
--
-- The values wrap around when reaching 'maxBound'.
addItemToClientStore :: (Enum ci, Bounded ci, Ord ci, Ord si, Ord a) => a -> ClientStore ci si a -> ClientStore ci si a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAdded cs
      newAddedItems =
        let newKey = findFreeSpot oldAddedItems
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAdded = newAddedItems}

-- | Find a free client id to use
--
-- You shouldn't need this function, 'addItemToClientStore' takes care of this.
--
-- The values wrap around when reaching 'maxBound'.
findFreeSpot :: (Ord ci, Enum ci, Bounded ci) => Map ci a -> ci
findFreeSpot m =
  if M.null m
    then minBound
    else
      let (i, _) = M.findMax m
       in go (next i)
  where
    go i =
      if M.member i m
        then go (next i)
        else i
    next ci
      | ci == maxBound = minBound
      | otherwise = succ ci

deleteUnsyncedFromClientStore :: (Ord ci, Ord si, Ord a) => ci -> ClientStore ci si a -> ClientStore ci si a
deleteUnsyncedFromClientStore cid cs = cs {clientStoreAdded = M.delete cid $ clientStoreAdded cs}

deleteSyncedFromClientStore :: (Ord ci, Ord si, Ord a) => si -> ClientStore ci si a -> ClientStore ci si a
deleteSyncedFromClientStore i cs =
  let syncedBefore = clientStoreSynced cs
   in case M.lookup i syncedBefore of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSynced = M.delete i syncedBefore,
              clientStoreDeleted = S.insert i $ clientStoreDeleted cs
            }

-- | A synchronisation request for items with Client Id's of type @ci@, Server Id's of type @i@ and values of type @a@
data SyncRequest ci si a
  = SyncRequest
      { syncRequestAdded :: !(Map ci a),
        syncRequestSynced :: !(Set si),
        syncRequestDeleted :: !(Set si)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData ci, NFData si, NFData a) => NFData (SyncRequest ci si a)

instance (Validity ci, Validity si, Validity a, Ord ci, Ord si, Ord a, Show ci) => Validity (SyncRequest ci si a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr,
        declare "the sync request items have distinct ids"
          $ distinct
          $ S.toList syncRequestSynced ++ S.toList syncRequestDeleted
      ]

instance (FromJSON ci, FromJSON si, FromJSON a, FromJSONKey ci, Ord ci, Ord si, Ord a) => FromJSON (SyncRequest ci si a) where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .: "added" <*> o .: "synced" <*> o .: "undeleted"

instance (ToJSON ci, ToJSON si, ToJSON a, ToJSONKey ci) => ToJSON (SyncRequest ci si a) where
  toJSON SyncRequest {..} =
    object
      [ "added" .= syncRequestAdded,
        "synced" .= syncRequestSynced,
        "undeleted" .= syncRequestDeleted
      ]

emptySyncRequest :: SyncRequest ci si a
emptySyncRequest =
  SyncRequest
    { syncRequestAdded = M.empty,
      syncRequestSynced = S.empty,
      syncRequestDeleted = S.empty
    }

-- | Produce a synchronisation request for a client-side store.
--
-- This request can then be sent to a central store for synchronisation.
makeSyncRequest :: (Ord ci, Ord si, Ord a) => ClientStore ci si a -> SyncRequest ci si a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestAdded = clientStoreAdded,
      syncRequestSynced = M.keysSet clientStoreSynced,
      syncRequestDeleted = clientStoreDeleted
    }

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse ci si a
  = SyncResponse
      { syncResponseClientAdded :: !(Map ci si),
        syncResponseClientDeleted :: !(Set si),
        syncResponseServerAdded :: !(Map si a),
        syncResponseServerDeleted :: !(Set si)
      }
  deriving (Show, Eq, Ord, Generic)

instance (NFData ci, NFData si, NFData a) => NFData (SyncResponse ci si a)

instance (Validity ci, Validity si, Validity a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => Validity (SyncResponse ci si a) where
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

instance (Ord ci, Ord si, FromJSON ci, FromJSON si, FromJSONKey ci, FromJSONKey si, Ord a, FromJSON a) => FromJSON (SyncResponse ci si a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .: "client-added" <*> o .: "client-deleted" <*> o .: "server-added"
        <*> o
        .: "server-deleted"

instance (ToJSON ci, ToJSON si, ToJSONKey ci, ToJSONKey si, ToJSON a) => ToJSON (SyncResponse ci si a) where
  toJSON SyncResponse {..} =
    object
      [ "client-added" .= syncResponseClientAdded,
        "client-deleted" .= syncResponseClientDeleted,
        "server-added" .= syncResponseServerAdded,
        "server-deleted" .= syncResponseServerDeleted
      ]

emptySyncResponse :: SyncResponse ci si a
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty,
      syncResponseClientDeleted = S.empty,
      syncResponseServerAdded = M.empty,
      syncResponseServerDeleted = S.empty
    }

-- | Merge a synchronisation response back into a client-side store.
mergeSyncResponse ::
  forall ci si a.
  (Ord ci, Ord si, Ord a) =>
  ClientStore ci si a ->
  SyncResponse ci si a ->
  ClientStore ci si a
mergeSyncResponse s sr =
  flip execState s $
    mergeSyncResponseCustom
      pureClientSyncProcessor
      sr

pureClientSyncProcessor :: forall ci si a. (Ord ci, Ord si) => ClientSyncProcessor ci si a (State (ClientStore ci si a))
pureClientSyncProcessor =
  ClientSyncProcessor
    { clientSyncProcessorSyncServerAdded = \m -> modify $ \cs ->
        cs {clientStoreSynced = M.union (clientStoreSynced cs) (m `diffSet` clientStoreIds cs)},
      clientSyncProcessorSyncClientAdded = \addedItems -> modify $ \cs ->
        let oldAdded = clientStoreAdded cs
            oldSynced = clientStoreSynced cs
            go :: (Map ci a, Map si a) -> ci -> si -> (Map ci a, Map si a)
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

data ClientSyncProcessor ci si a m
  = ClientSyncProcessor
      { clientSyncProcessorSyncServerAdded :: Map si a -> m (),
        clientSyncProcessorSyncClientAdded :: Map ci si -> m (),
        clientSyncProcessorSyncServerDeleted :: Set si -> m (),
        clientSyncProcessorSyncClientDeleted :: Set si -> m ()
      }
  deriving (Generic)

mergeSyncResponseCustom :: Monad m => ClientSyncProcessor ci si a m -> SyncResponse ci si a -> m ()
mergeSyncResponseCustom ClientSyncProcessor {..} SyncResponse {..} = do
  -- The order here matters!
  clientSyncProcessorSyncServerAdded syncResponseServerAdded
  clientSyncProcessorSyncServerDeleted syncResponseServerDeleted
  clientSyncProcessorSyncClientDeleted syncResponseClientDeleted
  clientSyncProcessorSyncClientAdded syncResponseClientAdded

-- | A record of the basic operations that are necessary to build a synchronisation processor.
data ServerSyncProcessor ci si a m
  = ServerSyncProcessor
      { -- | Delete the items with an identifier in the given set, return the set that was indeed deleted or did not exist.
        -- In particular, return the identifiers of the items that the client should forget about.
        serverSyncProcessorDeleteMany :: Set si -> m (Set si),
        -- | Query the identifiers of the items that are in the given set but not in the store.
        serverSyncProcessorQueryNoLongerSynced :: Set si -> m (Set si),
        -- | Query the items that are in store, but not in the given set.
        serverSyncProcessorQueryNewRemote :: Set si -> m (Map si a),
        -- | Insert a set of items into the store.
        serverSyncProcessorInsertMany :: Map ci a -> m (Map ci si)
      }
  deriving (Generic)

-- | Process a server-side synchronisation request using a custom synchronisation processor
--
-- WARNING: The identifier generation function must produce newly unique identifiers such that each new item gets a unique identifier.
--
-- You can use this function with deterministically-random identifiers or incrementing identifiers.
processServerSyncCustom ::
  forall ci si a m.
  (Ord ci, Ord si, Ord a, Monad m) =>
  ServerSyncProcessor ci si a m ->
  SyncRequest ci si a ->
  m (SyncResponse ci si a)
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
    deleteUndeleted :: m (Set si)
    deleteUndeleted = serverSyncProcessorDeleteMany syncRequestDeleted
    syncItemsToBeDeletedLocally :: m (Set si)
    syncItemsToBeDeletedLocally = serverSyncProcessorQueryNoLongerSynced syncRequestSynced
    syncNewRemoteItems :: m (Map si a)
    syncNewRemoteItems = serverSyncProcessorQueryNewRemote syncRequestSynced
    syncAddedItems :: m (Map ci si)
    syncAddedItems = serverSyncProcessorInsertMany syncRequestAdded

-- | A central store of items with identifiers of type @i@ and values of type @a@
newtype ServerStore si a
  = ServerStore
      { serverStoreItems :: Map si a
      }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance (NFData si, NFData a) => NFData (ServerStore si a)

instance (Validity si, Validity a, Show si, Show a, Ord si, Ord a) => Validity (ServerStore si a)

-- | An empty central store to start with
emptyServerStore :: ServerStore si a
emptyServerStore = ServerStore {serverStoreItems = M.empty}

-- | Process a server-side synchronisation request using a server id generator
--
-- see 'processSyncCustom'
processServerSync ::
  forall m ci si a.
  (Ord ci, Ord si, Ord a, Monad m) =>
  m si ->
  ServerStore si a ->
  SyncRequest ci si a ->
  m (SyncResponse ci si a, ServerStore si a)
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
    deleteMany :: Set si -> StateT (ServerStore si a) m (Set si)
    deleteMany s = do
      modC (`diffSet` s)
      pure s
    queryNoLongerSynced :: Set si -> StateT (ServerStore si a) m (Set si)
    queryNoLongerSynced s = query ((s `S.difference`) . M.keysSet)
    queryNewRemote :: Set si -> StateT (ServerStore si a) m (Map si a)
    queryNewRemote s = query (`diffSet` s)
    query :: (Map si a -> b) -> StateT (ServerStore si a) m b
    query func = gets $ func . serverStoreItems
    insertMany :: Map ci a -> StateT (ServerStore si a) m (Map ci si)
    insertMany =
      traverse $ \a -> do
        u <- lift genUuid
        ins u a
        pure u
    ins :: si -> a -> StateT (ServerStore si a) m ()
    ins i val = modC $ M.insert i val
    modC :: (Map si a -> Map si a) -> StateT (ServerStore si a) m ()
    modC func = modify (\(ServerStore m) -> ServerStore $ func m)

diffSet :: Ord si => Map si a -> Set si -> Map si a
diffSet m s = m `M.difference` toMap s

toMap :: Set si -> Map si ()
toMap = M.fromSet (const ())

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)
