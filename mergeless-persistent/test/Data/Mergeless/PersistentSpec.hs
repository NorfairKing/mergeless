{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeless.PersistentSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Mergeless
import Data.Mergeless.Persistent
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

instance GenUnchecked ServerThing

instance GenValid ServerThing

spec :: Spec
spec = modifyMaxShrinks (const 0) $ persistentMergelessSpec $ do
  describe "Single client" $ do
    describe "Single item" $ do
      it "Succesfully downloads a single item from the server for an empty client" $ \te ->
        forAllValid $ \t -> runTest te $ do
          runServerDB $ insert_ (t :: ServerThing)
          req <- clientMakeSyncRequest
          sstore1 <- serverGetStore
          resp <- serverProcessSync req
          clientMergeSyncResponse resp
          cstore2 <- clientGetStore
          sstore2 <- serverGetStore
          lift $ do
            sstore2 `shouldBe` sstore1
            clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      pure ()
    describe "Multiple items" $ do
      pure ()
  describe "Two clients" $ do
    describe "Single item" $ do
      pure ()
    describe "Multiple items" $ do
      pure ()

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: SqlPersistT IO a -> T a
runClientDB func = do
  pool <- asks testEnvClientPool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

clientGetStore :: T (ClientStore ServerThingId ServerThing)
clientGetStore = runClientDB clientGetStoreQuery

clientMakeSyncRequest :: T (SyncRequest ServerThingId ServerThing)
clientMakeSyncRequest = runClientDB clientMakeSyncRequestQuery

serverGetStore :: T (ServerStore ServerThingId ServerThing)
serverGetStore = runServerDB serverGetStoreQuery

serverProcessSync :: SyncRequest ServerThingId ServerThing -> T (SyncResponse ServerThingId ServerThing)
serverProcessSync = runServerDB . serverProcessSyncQuery

clientMergeSyncResponse :: SyncResponse ServerThingId ServerThing -> T ()
clientMergeSyncResponse = runClientDB . clientMergeSyncResponseQuery

-- Things learnt:
-- the server thing needs to be different from the client thing
-- the server thing needs to be able to be different from the thing on the wire because it needs to be able to deal with users
-- the client needs to keep a client id.
--  They are only necessary for one client request/response roundtrip so they can be generated,
--  HOWEVER they need to be matched up when the response comes and there is nothing else to identify the items by
-- the client needs to keep the server id
-- the client needs to tombstone the deleted items

clientGetStoreQuery :: SqlPersistT IO (ClientStore ServerThingId ServerThing)
clientGetStoreQuery = do
  --let mkEntityMap :: [Entity v] -> Map (key v) v
  --    mkEntityMap = M.fromList . map (\(Entity vi v) -> (vi, v))
  clientStoreAdded <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId !=. Nothing, ClientThingServerId ==. Nothing, ClientThingDeleted ==. False] []
  clientStoreSynced <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingServerId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId ==. Nothing, ClientThingServerId !=. Nothing, ClientThingDeleted ==. False] []
  clientStoreDeleted <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingServerId !=. Nothing, ClientThingDeleted ==. True] []
  pure ClientStore {..}

clientMakeSyncRequestQuery :: SqlPersistT IO (SyncRequest ServerThingId ServerThing)
clientMakeSyncRequestQuery = do
  syncRequestAdded <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId !=. Nothing, ClientThingServerId ==. Nothing, ClientThingDeleted ==. False] []
  syncRequestSynced <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingClientId ==. Nothing, ClientThingServerId !=. Nothing, ClientThingDeleted ==. False] []
  syncRequestDeleted <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingServerId !=. Nothing, ClientThingDeleted ==. True] []
  pure SyncRequest {..}

serverGetStoreQuery :: SqlPersistT IO (ServerStore ServerThingId ServerThing)
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, st)) <$> selectList [] []

serverProcessSyncQuery :: SyncRequest ServerThingId ServerThing -> SqlPersistT IO (SyncResponse ServerThingId ServerThing)
serverProcessSyncQuery sr = do
  let serverSyncProcessorDeleteMany s = do
        deleteWhere [ServerThingId <-. S.toList s]
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced s = do
        aliases <-
          selectList [ServerThingId <-. S.toList s] []
        let inSButNotInStore =
              s `S.difference` S.fromList (map entityKey aliases)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (\(Entity ti t) -> (ti, t))
          <$> selectList [ServerThingId /<-. S.toList s] []
      serverSyncProcessorInsertMany m =
        fmap (M.fromList . catMaybes)
          $ forM (M.toList m)
          $ \(cid, t) -> do
            mid <- insertUnique t
            pure $ (,) cid <$> mid
      proc = ServerSyncProcessor {..}
  processServerSyncCustom proc sr

clientMergeSyncResponseQuery :: SyncResponse ServerThingId ServerThing -> SqlPersistT IO ()
clientMergeSyncResponseQuery sr = do
  let clientSyncProcessorSyncServerAdded m = void $ flip M.traverseWithKey m $ \si ServerThing {..} ->
        insert_ $
          ClientThing
            { clientThingNumber = serverThingNumber,
              clientThingClientId = Nothing,
              clientThingServerId = Just si,
              clientThingDeleted = False
            }
      clientSyncProcessorSyncClientAdded m = void $ flip M.traverseWithKey m $ \cid sid ->
        updateWhere [ClientThingClientId ==. Just cid] [ClientThingClientId =. Nothing, ClientThingServerId =. Just sid]
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      proc = ClientSyncProcessor {..}
  mergeSyncResponseCustom proc sr
