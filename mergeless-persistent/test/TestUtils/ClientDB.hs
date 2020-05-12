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

import Control.Monad
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeless
import Data.Mergeless.Persistent ()
import qualified Data.Set as S
import Database.Persist
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

setupUnsyncedClientQuery :: [ServerThing] -> SqlPersistT IO ()
setupUnsyncedClientQuery =
  foldM_ go minBound
  where
    go :: Int64 -> ServerThing -> SqlPersistT IO Int64
    go cid ServerThing {..} = do
      insertKey
        (toSqlKey cid)
        ClientThing
          { clientThingNumber = serverThingNumber,
            clientThingServerId = Nothing,
            clientThingDeleted = False
          }
      pure $ succ cid

setupClientQuery :: ClientStore ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
setupClientQuery ClientStore {..} = do
  forM_ (M.toList clientStoreAdded) $ \(cid, ServerThing {..}) ->
    insertKey
      cid
      ClientThing
        { clientThingNumber = serverThingNumber,
          clientThingServerId = Nothing,
          clientThingDeleted = False
        }
  forM_ (M.toList clientStoreSynced) $ \(sid, ServerThing {..}) ->
    insert_
      ClientThing
        { clientThingNumber = serverThingNumber,
          clientThingServerId = Just sid,
          clientThingDeleted = False
        }
  forM_ (S.toList clientStoreDeleted) $ \sid ->
    insert_
      ClientThing
        { clientThingNumber = 0, -- Dummy value
          clientThingServerId = Just sid,
          clientThingDeleted = True
        }

clientGetStoreQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId ServerThing)
clientGetStoreQuery = do
  clientStoreAdded <-
    M.fromList . map (\(Entity cid ClientThing {..}) -> (cid, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingServerId ==. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  clientStoreSynced <-
    M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingServerId, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  clientStoreDeleted <-
    S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId)
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingDeleted ==. True
        ]
        []
  pure ClientStore {..}

clientMakeSyncRequestQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId ServerThing)
clientMakeSyncRequestQuery = do
  syncRequestAdded <-
    M.fromList . map (\(Entity cid ClientThing {..}) -> (cid, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingServerId ==. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  syncRequestSynced <-
    S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId)
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  syncRequestDeleted <-
    S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId)
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingDeleted ==. True
        ]
        []
  pure SyncRequest {..}

clientMergeSyncResponseQuery :: SyncResponse ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
clientMergeSyncResponseQuery sr = do
  let clientSyncProcessorSyncServerAdded m = forM_ (M.toList m) $ \(si, ServerThing {..}) ->
        insert_
          ( ClientThing
              { clientThingNumber = serverThingNumber,
                clientThingServerId = Just si,
                clientThingDeleted = False
              }
          )
      clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(cid, sid) ->
        update cid [ClientThingServerId =. Just sid]
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid, ClientThingDeleted ==. True]
      proc = ClientSyncProcessor {..}
  mergeSyncResponseCustom proc sr
