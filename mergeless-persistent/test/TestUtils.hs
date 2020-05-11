{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtils
  ( module TestUtils,
    module TestUtils.ServerDB,
    module TestUtils.ClientDB,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.GenValidity
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Path
import Path.IO
import Test.Hspec
import TestUtils.ClientDB
import TestUtils.ServerDB

instance Validity (Key a) where
  validate = trivialValidation

instance Validity a => Validity (Entity a) where
  validate e = mconcat [delve "entityKey" $ entityKey e, delve "entityVal" $ entityVal e]

instance ToBackendKey SqlBackend record => GenUnchecked (Key record) where
  genUnchecked = toSqlKey <$> genUnchecked
  shrinkUnchecked = fmap toSqlKey . shrinkValid . fromSqlKey

instance ToBackendKey SqlBackend record => GenValid (Key record) where
  genValid = toSqlKey <$> genValid
  shrinkValid = shrinkUnchecked

data TestEnv
  = TestEnv
      { testEnvClientPool :: ConnectionPool,
        testEnvServerPool :: ConnectionPool
      }

persistentMergelessSpec :: SpecWith TestEnv -> Spec
persistentMergelessSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  runNoLoggingT
    $ withSystemTempFile "mergeless-persistent-test-client-db"
    $ \clientFile _ ->
      withSystemTempFile "mergeless-persistent-test-server-db" $ \serverFile _ ->
        withSqlitePool (T.pack $ fromAbsFile clientFile) 1 $
          \clientPool ->
            withSqlitePool (T.pack $ fromAbsFile serverFile) 1 $
              \serverPool -> do
                let tenv = TestEnv {testEnvClientPool = clientPool, testEnvServerPool = serverPool}
                flip runSqlPool clientPool $ void $ runMigrationSilent migrateClient
                flip runSqlPool serverPool $ void $ runMigrationSilent migrateServer
                liftIO $ func tenv
