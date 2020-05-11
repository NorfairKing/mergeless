{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils
  ( module TestUtils,
    module TestUtils.ServerDB,
    module TestUtils.ClientDB,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Path
import Path.IO
import Test.Hspec
import TestUtils.ClientDB
import TestUtils.ServerDB

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
