{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Control.Monad
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeless
import qualified Data.Set as S
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateServer"]
  [persistLowerCase|

ServerThing
  number Int

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ServerThing

instance GenUnchecked ServerThing

instance GenValid ServerThing

setupServerQuery :: ServerStore ServerThingId ServerThing -> SqlPersistT IO ()
setupServerQuery ServerStore {..} = forM_ (M.toList serverStoreItems) $ \(i, e) -> void $ insertKey i e

serverGetStoreQuery :: SqlPersistT IO (ServerStore ServerThingId ServerThing)
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, st)) <$> selectList [] []

serverProcessSyncQuery :: SyncRequest ServerThingId ServerThing -> SqlPersistT IO (SyncResponse ServerThingId ServerThing)
serverProcessSyncQuery sr = do
  let serverSyncProcessorDeleteMany s = do
        deleteWhere [ServerThingId <-. S.toList s] -- FIXME this operator can crash
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced s = do
        aliases <-
          selectList [ServerThingId <-. S.toList s] [] -- FIXME this operator can crash
        let inSButNotInStore =
              s `S.difference` S.fromList (map entityKey aliases)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (\(Entity ti t) -> (ti, t))
          <$> selectList [ServerThingId /<-. S.toList s] [] -- FIXME this operator can crash
      serverSyncProcessorInsertMany m =
        fmap (M.fromList . catMaybes)
          $ forM (M.toList m)
          $ \(cid, t) -> do
            mid <- insertUnique t
            pure $ (,) cid <$> mid
      proc = ServerSyncProcessor {..}
  processServerSyncCustom proc sr
