{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeless.Persistent
  ( -- * Server side
    serverProcessSyncQuery,

    -- ** Utils
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

-- | Process a sync query on the server side.
serverProcessSyncQuery ::
  ( Ord ci,
    Ord record,
    PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  EntityField record (Key record) ->
  SyncRequest ci (Key record) record ->
  SqlPersistT IO (SyncResponse ci (Key record) record)
serverProcessSyncQuery idField sr = do
  let serverSyncProcessorDeleteMany s = do
        deleteWhere [idField <-. S.toList s] -- FIXME this operator can crash
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced s = do
        aliases <-
          selectList [idField <-. S.toList s] [] -- FIXME this operator can crash
        let inSButNotInStore =
              s `S.difference` S.fromList (map entityKey aliases)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (\(Entity ti t) -> (ti, t))
          <$> selectList [idField /<-. S.toList s] [] -- FIXME this operator can crash
      serverSyncProcessorInsertMany m =
        fmap (M.fromList . catMaybes)
          $ forM (M.toList m)
          $ \(cid, t) -> do
            mid <- insertUnique t
            pure $ (,) cid <$> mid
      proc = ServerSyncProcessor {..}
  processServerSyncCustom proc sr

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
