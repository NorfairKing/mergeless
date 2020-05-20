{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils.ClientDB where

import Data.Mergeless
import Data.Mergeless.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  number Int
  serverId ServerThingId Maybe -- Nothing means it's not been synced
  deleted Bool -- True means this item has been tombstoned

  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

setupUnsyncedClientThingQuery :: [Thing] -> SqlPersistT IO ()
setupUnsyncedClientThingQuery = setupUnsyncedClientQuery makeUnsyncedClientThing

setupClientThingQuery :: ClientStore ClientThingId ServerThingId Thing -> SqlPersistT IO ()
setupClientThingQuery = setupClientQuery makeUnsyncedClientThing makeSyncedClientThing makeDeletedClientThing

clientGetStoreThingQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId Thing)
clientGetStoreThingQuery = clientGetStoreQuery clientMakeThing ClientThingServerId ClientThingDeleted

clientMakeSyncRequestThingQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId Thing)
clientMakeSyncRequestThingQuery = clientMakeSyncRequestQuery clientMakeThing ClientThingServerId ClientThingDeleted

clientMergeSyncResponseThingQuery :: SyncResponse ClientThingId ServerThingId Thing -> SqlPersistT IO ()
clientMergeSyncResponseThingQuery = clientMergeSyncResponseQuery makeSyncedClientThing ClientThingServerId ClientThingDeleted

makeUnsyncedClientThing :: Thing -> ClientThing
makeUnsyncedClientThing Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingDeleted = False,
      clientThingServerId = Nothing
    }

makeSyncedClientThing :: ServerThingId -> Thing -> ClientThing
makeSyncedClientThing sid Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingDeleted = False,
      clientThingServerId = Just sid
    }

makeDeletedClientThing :: ServerThingId -> ClientThing
makeDeletedClientThing sid =
  ClientThing
    { clientThingNumber = 0, -- dummy
      clientThingDeleted = True,
      clientThingServerId = Just sid
    }

clientMakeThing :: ClientThing -> Thing
clientMakeThing ClientThing {..} = Thing {thingNumber = clientThingNumber}
