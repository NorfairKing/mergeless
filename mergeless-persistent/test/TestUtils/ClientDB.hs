{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

setupUnsyncedClientThingQuery :: [ServerThing] -> SqlPersistT IO ()
setupUnsyncedClientThingQuery = setupUnsyncedClientQuery makeUnsyncedClientThing

setupClientThingQuery :: ClientStore ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
setupClientThingQuery = setupClientQuery makeUnsyncedClientThing makeSyncedClientThing makeDeletedClientThing

clientGetStoreThingQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId ServerThing)
clientGetStoreThingQuery = clientGetStoreQuery makeServerThing ClientThingServerId ClientThingDeleted

clientMakeSyncRequestThingQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId ServerThing)
clientMakeSyncRequestThingQuery = clientMakeSyncRequestQuery makeServerThing ClientThingServerId ClientThingDeleted

clientMergeSyncResponseThingQuery :: SyncResponse ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
clientMergeSyncResponseThingQuery = clientMergeSyncResponseQuery makeSyncedClientThing ClientThingServerId ClientThingDeleted

makeUnsyncedClientThing :: ServerThing -> ClientThing
makeUnsyncedClientThing ServerThing {..} =
  ClientThing
    { clientThingNumber = serverThingNumber,
      clientThingDeleted = False,
      clientThingServerId = Nothing
    }

makeSyncedClientThing :: ServerThingId -> ServerThing -> ClientThing
makeSyncedClientThing sid ServerThing {..} =
  ClientThing
    { clientThingNumber = serverThingNumber,
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
