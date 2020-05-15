{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Data.GenValidity
import Data.Mergeless
import Data.Mergeless.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

newtype Thing
  = Thing
      { thingNumber :: Int
      }
  deriving (Show, Eq, Generic)

instance Validity Thing

instance GenUnchecked Thing

instance GenValid Thing

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

setupServerThingQuery :: ServerStore ServerThingId Thing -> SqlPersistT IO ()
setupServerThingQuery = setupServerQuery makeServerThing

serverGetStoreThingQuery :: SqlPersistT IO (ServerStore ServerThingId Thing)
serverGetStoreThingQuery = serverGetStoreQuery serverMakeThing

serverProcessSyncThingQuery :: Ord cid => SyncRequest cid ServerThingId Thing -> SqlPersistT IO (SyncResponse cid ServerThingId Thing)
serverProcessSyncThingQuery = serverProcessSyncQuery serverMakeThing makeServerThing

serverMakeThing :: ServerThing -> Thing
serverMakeThing ServerThing {..} = Thing {thingNumber = serverThingNumber}

makeServerThing :: Thing -> ServerThing
makeServerThing Thing {..} = ServerThing {serverThingNumber = thingNumber}
