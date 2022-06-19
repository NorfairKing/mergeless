{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.GenValidity.Persist ()
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import TestUtils.ClientDB
import TestUtils.ServerDB

withServerPool :: (ConnectionPool -> IO a) -> IO a
withServerPool func =
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \serverPool -> do
      flip runSqlPool serverPool $ void $ runMigrationQuiet migrateServer
      liftIO $ func serverPool

withClientPool :: (ConnectionPool -> IO a) -> IO a
withClientPool func =
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \clientPool -> do
      flip runSqlPool clientPool $ void $ runMigrationQuiet migrateClient
      liftIO $ func clientPool
