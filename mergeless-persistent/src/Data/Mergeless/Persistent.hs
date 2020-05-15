{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeless.Persistent
  ( -- * Client side
    clientMakeSyncRequestQuery,
    clientMergeSyncResponseQuery,

    -- * Server side
    serverProcessSyncQuery,

    -- ** Sync Processor
    serverSyncProcessor,

    -- * Utils
    setupUnsyncedClientQuery,
    setupClientQuery,
    clientGetStoreQuery,
    serverGetStoreQuery,
    setupServerQuery,
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeless
import qualified Data.Set as S
import Database.Persist
import Database.Persist.Sql
import Lens.Micro

-- | Make a sync request on the client side
clientMakeSyncRequestQuery ::
  ( Ord (Key serverRecord),
    PersistEntity clientRecord,
    PersistField (Key clientRecord),
    PersistField (Key serverRecord),
    PersistEntityBackend clientRecord ~ SqlBackend,
    PersistEntityBackend serverRecord ~ SqlBackend
  ) =>
  (clientRecord -> serverRecord) ->
  EntityField clientRecord (Maybe (Key serverRecord)) ->
  EntityField clientRecord Bool ->
  SqlPersistT IO (SyncRequest (Key clientRecord) (Key serverRecord) serverRecord)
clientMakeSyncRequestQuery func serverIdField deletedField = do
  syncRequestAdded <-
    M.fromList . map (\(Entity cid ct) -> (cid, func ct))
      <$> selectList
        [ serverIdField ==. Nothing,
          deletedField ==. False
        ]
        []
  syncRequestSynced <-
    S.fromList . map (\e -> fromJust (e ^. fieldLens serverIdField))
      <$> selectList
        [ serverIdField !=. Nothing,
          deletedField ==. False
        ]
        []
  syncRequestDeleted <-
    S.fromList . map (\e -> fromJust (e ^. fieldLens serverIdField))
      <$> selectList
        [ serverIdField !=. Nothing,
          deletedField ==. True
        ]
        []
  pure SyncRequest {..}

-- | Merge a sync response on the client side
clientMergeSyncResponseQuery ::
  ( Ord (Key serverRecord),
    PersistEntity clientRecord,
    PersistField (Key clientRecord),
    PersistField (Key serverRecord),
    PersistEntityBackend clientRecord ~ SqlBackend,
    PersistEntityBackend serverRecord ~ SqlBackend
  ) =>
  -- | Create an un-deleted synced record on the client side
  (Key serverRecord -> serverRecord -> clientRecord) ->
  EntityField clientRecord (Maybe (Key serverRecord)) ->
  EntityField clientRecord Bool ->
  SyncResponse (Key clientRecord) (Key serverRecord) serverRecord ->
  SqlPersistT IO ()
clientMergeSyncResponseQuery func serverIdField deletedField sr = do
  let clientSyncProcessorSyncServerAdded m = forM_ (M.toList m) $ \(si, st) ->
        insert_ $ func si st
      clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(cid, sid) ->
        update cid [serverIdField =. Just sid]
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [serverIdField ==. Just sid]
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [serverIdField ==. Just sid, deletedField ==. True]
      proc = ClientSyncProcessor {..}
  mergeSyncResponseCustom proc sr

-- | Process a sync query on the server side.
serverProcessSyncQuery ::
  ( Ord ci,
    Ord record,
    PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  SyncRequest ci (Key record) record ->
  SqlPersistT IO (SyncResponse ci (Key record) record)
serverProcessSyncQuery = processServerSyncCustom serverSyncProcessor

serverSyncProcessor ::
  ( Ord ci,
    Ord record,
    PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  ServerSyncProcessor ci (Key record) record (SqlPersistT IO)
serverSyncProcessor =
  ServerSyncProcessor {..}
  where
    serverSyncProcessorRead = M.fromList . map (\(Entity i record) -> (i, record)) <$> selectList [] []
    serverSyncProcessorAddItems = mapM insert
    serverSyncProcessorDeleteItems s = do
      mapM_ delete s
      pure s

-- | Setup an unsynced client store
--
-- You shouldn't need this.
setupUnsyncedClientQuery ::
  (PersistEntity clientRecord, PersistEntityBackend clientRecord ~ SqlBackend) =>
  (serverRecord -> clientRecord) ->
  [serverRecord] ->
  SqlPersistT IO ()
setupUnsyncedClientQuery func = mapM_ (insert . func)

-- | Setup a client store
--
-- You shouldn't need this.
setupClientQuery ::
  ( Ord (Key serverRecord),
    PersistEntity clientRecord,
    PersistField (Key clientRecord),
    PersistField (Key serverRecord),
    PersistEntityBackend clientRecord ~ SqlBackend,
    PersistEntityBackend serverRecord ~ SqlBackend
  ) =>
  -- | Create an un-deleted unsynced record on the client side
  (serverRecord -> clientRecord) ->
  -- | Create an un-deleted synced record on the client side
  (Key serverRecord -> serverRecord -> clientRecord) ->
  -- | Create an deleted synced record on the client side
  (Key serverRecord -> clientRecord) ->
  ClientStore (Key clientRecord) (Key serverRecord) serverRecord ->
  SqlPersistT IO ()
setupClientQuery funcU funcS funcD ClientStore {..} = do
  forM_ (M.toList clientStoreAdded) $ \(cid, st) ->
    insertKey
      cid
      (funcU st)
  forM_ (M.toList clientStoreSynced) $ \(sid, st) ->
    insert_ (funcS sid st)
  forM_ (S.toList clientStoreDeleted) $ \sid ->
    insert_ (funcD sid)

-- | Get a client store
--
-- You shouldn't need this.
clientGetStoreQuery ::
  ( Ord (Key serverRecord),
    PersistEntity clientRecord,
    PersistField (Key clientRecord),
    PersistField (Key serverRecord),
    PersistEntityBackend clientRecord ~ SqlBackend,
    PersistEntityBackend serverRecord ~ SqlBackend
  ) =>
  (clientRecord -> serverRecord) ->
  EntityField clientRecord (Maybe (Key serverRecord)) ->
  EntityField clientRecord Bool ->
  SqlPersistT IO (ClientStore (Key clientRecord) (Key serverRecord) serverRecord)
clientGetStoreQuery func serverIdField deletedField = do
  clientStoreAdded <-
    M.fromList . map (\(Entity cid ct) -> (cid, func ct))
      <$> selectList
        [ serverIdField ==. Nothing,
          deletedField ==. False
        ]
        []
  clientStoreSynced <-
    M.fromList . map (\e@(Entity _ ct) -> (fromJust (e ^. fieldLens serverIdField), func ct))
      <$> selectList
        [ serverIdField !=. Nothing,
          deletedField ==. False
        ]
        []
  clientStoreDeleted <-
    S.fromList . map (\e -> fromJust (e ^. fieldLens serverIdField))
      <$> selectList
        [ serverIdField !=. Nothing,
          deletedField ==. True
        ]
        []
  pure ClientStore {..}

-- | Get the server store from the database
--
-- You shouldn't need this.
serverGetStoreQuery ::
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  SqlPersistT IO (ServerStore (Key record) record)
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, st)) <$> selectList [] []

-- | Set up a server store in the database.
--
-- You shouldn't need this.
-- This uses 'insertKey' function and is therefore unsafe.
setupServerQuery ::
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  ServerStore (Key record) record ->
  SqlPersistT IO ()
setupServerQuery ServerStore {..} = forM_ (M.toList serverStoreItems) $ \(i, e) -> void $ insertKey i e
