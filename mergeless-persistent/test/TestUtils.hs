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
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Path
import Path.IO
import TestUtils.ClientDB
import TestUtils.ServerDB

withServerPool :: (ConnectionPool -> IO a) -> IO a
withServerPool func =
  runNoLoggingT $ withSqlitePool ":memory:" 1 $ \serverPool -> do
    flip runSqlPool serverPool $ void $ runMigrationSilent migrateServer
    liftIO $ func serverPool

withClientPool :: Int -> (ConnectionPool -> IO a) -> IO a
withClientPool i func =
  runNoLoggingT $ withSqlitePool ":memory:" 1 $ \clientPool -> do
    flip runSqlPool clientPool $ void $ runMigrationSilent migrateClient
    liftIO $ func clientPool
