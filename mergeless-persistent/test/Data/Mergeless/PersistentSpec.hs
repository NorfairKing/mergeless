{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeless.PersistentSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.GenValidity.Mergeless ()
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeless
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ persistentMergelessSpec
  $ describe "Single client"
  $ do
    describe "Single item" $ do
      it "Succesfully downloads a single item from the server for an empty client" $ \te ->
        forAllValid $ \si -> runTest te $ do
          setupServer [si]
          (_, sstore1, sstore2, cstore2) <- sync
          liftIO $ do
            sstore2 `shouldBe` sstore1
            clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads a single item to the server for an empty server" $ \te ->
        forAllValid $ \si ->
          runTest te $
            do
              setupUnsyncedClient [si]
              (_, _, sstore2, cstore2) <- sync
              liftIO $ do
                clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
                sort (M.elems (clientStoreSynced cstore2))
                  `shouldBe` [si]
    describe "Multiple items" $ do
      it "succesfully downloads everything from the server for an empty client" $ \te -> forAllValid $ \sis ->
        runTest te $ do
          setupServer sis
          (_, sstore1, sstore2, cstore2) <- sync
          liftIO $ do
            sstore2 `shouldBe` sstore1
            clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server" $ \te -> forAllValid $ \sis ->
        runTest te $ do
          setupUnsyncedClient sis
          (_, _, sstore2, cstore2) <- sync
          liftIO $ do
            clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
            sort (M.elems (clientStoreSynced cstore2)) `shouldBe` sort sis
      it "is idempotent with one client" $ \te -> forAllValid $ \sis -> forAllValid $ \cs ->
        runTest te $ do
          setupServer sis
          setupClient cs
          (_, _, sstore2, cstore2) <- sync
          (_, _, sstore3, cstore3) <- sync
          liftIO $ do
            cstore3 `shouldBe` cstore2
            sstore3 `shouldBe` sstore2

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

setupServer :: [ServerThing] -> T ()
setupServer =
  runServerDB . mapM_ insert_ . S.fromList

setupUnsyncedClient :: [ServerThing] -> T ()
setupUnsyncedClient sts =
  runClientDB $ foldM_ go minBound sts
  where
    go :: ClientId -> ServerThing -> SqlPersistT IO ClientId
    go cid ServerThing {..} = do
      insert_ ClientThing {clientThingNumber = serverThingNumber, clientThingClientId = Just cid, clientThingServerId = Nothing, clientThingDeleted = False}
      pure $ succ cid

type CS = ClientStore ServerThingId ServerThing

type SReq = SyncRequest ServerThingId ServerThing

type SS = ServerStore ServerThingId ServerThing

type SResp = SyncResponse ServerThingId ServerThing

sync :: T (CS, SS, SS, CS)
sync = do
  cstore1 <- clientGetStore
  req <- clientMakeSyncRequest
  sstore1 <- serverGetStore
  resp <- serverProcessSync req
  sstore2 <- serverGetStore
  clientMergeSyncResponse resp
  cstore2 <- clientGetStore
  pure (cstore1, sstore1, sstore2, cstore2)

setupClient :: CS -> T ()
setupClient = runClientDB . setupClientQuery

clientGetStore :: T CS
clientGetStore = runClientDB clientGetStoreQuery

clientMakeSyncRequest :: T SReq
clientMakeSyncRequest = runClientDB clientMakeSyncRequestQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncQuery

clientMergeSyncResponse :: SResp -> T ()
clientMergeSyncResponse = runClientDB . clientMergeSyncResponseQuery

setupClientQuery :: CS -> SqlPersistT IO ()
setupClientQuery ClientStore {..} = do
  forM_ (M.toList clientStoreAdded) $ \(cid, ServerThing {..}) -> insert_ ClientThing {clientThingNumber = serverThingNumber, clientThingClientId = Just cid, clientThingServerId = Nothing, clientThingDeleted = False}
  forM_ (M.toList clientStoreSynced) $ \(sid, ServerThing {..}) -> insert_ ClientThing {clientThingNumber = serverThingNumber, clientThingClientId = Nothing, clientThingServerId = Just sid, clientThingDeleted = False}
  forM_ (S.toList clientStoreDeleted) $ \sid ->
    insert_
      ClientThing
        { clientThingNumber = 0, -- Dummy value
          clientThingClientId = Nothing,
          clientThingServerId = Just sid,
          clientThingDeleted = True
        }

-- Things learnt:
-- the server thing needs to be different from the client thing
-- the server thing needs to be able to be different from the thing on the wire because it needs to be able to deal with users
-- the client needs to keep a client id.
--  They are only necessary for one client request/response roundtrip so they can be generated,
--  HOWEVER they need to be matched up when the response comes and there is nothing else to identify the items by
-- the client needs to keep the server id
-- the client needs to tombstone the deleted items

clientGetStoreQuery :: SqlPersistT IO CS
clientGetStoreQuery = do
  --let mkEntityMap :: [Entity v] -> Map (key v) v
  --    mkEntityMap = M.fromList . map (\(Entity vi v) -> (vi, v))
  clientStoreAdded <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId !=. Nothing, ClientThingServerId ==. Nothing, ClientThingDeleted ==. False] []
  clientStoreSynced <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingServerId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId ==. Nothing, ClientThingServerId !=. Nothing, ClientThingDeleted ==. False] []
  clientStoreDeleted <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingServerId !=. Nothing, ClientThingDeleted ==. True] []
  pure ClientStore {..}

clientMakeSyncRequestQuery :: SqlPersistT IO SReq
clientMakeSyncRequestQuery = do
  syncRequestAdded <- M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber})) <$> selectList [ClientThingClientId !=. Nothing, ClientThingServerId ==. Nothing, ClientThingDeleted ==. False] []
  syncRequestSynced <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingClientId ==. Nothing, ClientThingServerId !=. Nothing, ClientThingDeleted ==. False] []
  syncRequestDeleted <- S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId) <$> selectList [ClientThingServerId !=. Nothing, ClientThingDeleted ==. True] []
  pure SyncRequest {..}

serverGetStoreQuery :: SqlPersistT IO SS
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, st)) <$> selectList [] []

serverProcessSyncQuery :: SReq -> SqlPersistT IO SResp
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

clientMergeSyncResponseQuery :: SResp -> SqlPersistT IO ()
clientMergeSyncResponseQuery sr = do
  let clientSyncProcessorSyncServerAdded m = void $ flip M.traverseWithKey m $ \si ServerThing {..} ->
        upsertBy
          (ClientUniqueServerId $ Just si)
          ( ClientThing
              { clientThingNumber = serverThingNumber,
                clientThingClientId = Nothing,
                clientThingServerId = Just si,
                clientThingDeleted = False
              }
          )
          [ClientThingNumber =. serverThingNumber, ClientThingClientId =. Nothing, ClientThingServerId =. Just si, ClientThingDeleted =. False]
      clientSyncProcessorSyncClientAdded m = void $ flip M.traverseWithKey m $ \cid sid ->
        updateWhere [ClientThingClientId ==. Just cid] [ClientThingClientId =. Nothing, ClientThingServerId =. Just sid]
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      proc = ClientSyncProcessor {..}
  mergeSyncResponseCustom proc sr
