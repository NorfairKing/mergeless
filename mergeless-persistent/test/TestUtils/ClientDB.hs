{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ClientDB where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Mergeless
import Data.Mergeless.Persistent ()
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
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  number Int
  clientId ClientId Maybe -- Nothing means it's been synced
  serverId ServerThingId Maybe -- Nothing means it's not been synced
  deleted Bool -- True means this item has been tombstoned

  ClientUniqueClientId clientId !force
  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]
-- instance Validity ClientThing
