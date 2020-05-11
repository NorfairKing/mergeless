{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- Things learnt:
-- the server thing needs to be different from the client thing
-- the server thing needs to be able to be different from the thing on the wire because it needs to be able to deal with users
-- the client needs to keep a client id.
--  They are only necessary for one client request/response roundtrip so they can be generated,
--  HOWEVER they need to be matched up when the response comes and there is nothing else to identify the items by
-- the client needs to keep the server id
-- the client needs to tombstone the deleted items

module Data.Mergeless.Persistent.SingleClientSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.GenValidity.Mergeless
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeless
import Data.Set (Set)
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ persistentMergelessSpec $ do
  describe "Single item" $ do
    it "Succesfully downloads a single item from the server for an empty client" $ \te ->
      forAllValid $ \(sid, si) -> runTest te $ do
        setupServer $ ServerStore $ M.singleton sid si
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
    it "produces valid stores" $ \te -> forAllValid $ \sids ->
      forAll (genClientStoreFromSet sids) $ \cs ->
        forAll (genServerStoreFromSet sids) $ \ss ->
          runTest te $ do
            setupServer ss
            setupClient cs
            (_, _, cstore2, sstore2) <- sync
            liftIO $ do
              shouldBeValid cstore2
              shouldBeValid sstore2
    it "is idempotent with one client" $ \te -> forAllValid $ \sids ->
      forAll (genClientStoreFromSet sids) $ \cs ->
        forAll (genServerStoreFromSet sids) $ \ss ->
          runTest te $ do
            setupServer ss
            setupClient cs
            void sync
            (cstore2, sstore2, sstore3, cstore3) <- sync
            liftIO $ do
              cstore3 `shouldBe` cstore2
              sstore3 `shouldBe` sstore2

genServerStoreFromSet :: (Ord i, GenValid v) => Set i -> Gen (ServerStore i v)
genServerStoreFromSet s = ServerStore <$> mapWithIds s

genClientStoreFromSet :: (Ord i, GenValid v) => Set i -> Gen (ClientStore i v)
genClientStoreFromSet s = do
  (s1, s2) <- splitSet s
  clientStoreAdded <- genValid
  clientStoreSynced <- mapWithIds s1
  let clientStoreDeleted = s2
  pure ClientStore {..}

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

setupUnsyncedClient :: [ServerThing] -> T ()
setupUnsyncedClient sts =
  runClientDB $ foldM_ go minBound sts
  where
    go :: ClientId -> ServerThing -> SqlPersistT IO ClientId
    go cid ServerThing {..} = do
      insert_
        ClientThing
          { clientThingNumber = serverThingNumber,
            clientThingClientId = Just cid,
            clientThingServerId = Nothing,
            clientThingDeleted = False
          }
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

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerQuery

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
  forM_ (M.toList clientStoreAdded) $ \(cid, ServerThing {..}) ->
    insert_
      ClientThing
        { clientThingNumber = serverThingNumber,
          clientThingClientId = Just cid,
          clientThingServerId = Nothing,
          clientThingDeleted = False
        }
  forM_ (M.toList clientStoreSynced) $ \(sid, ServerThing {..}) ->
    insert_
      ClientThing
        { clientThingNumber = serverThingNumber,
          clientThingClientId = Nothing,
          clientThingServerId = Just sid,
          clientThingDeleted = False
        }
  forM_ (S.toList clientStoreDeleted) $ \sid ->
    insert_
      ClientThing
        { clientThingNumber = 0, -- Dummy value
          clientThingClientId = Nothing,
          clientThingServerId = Just sid,
          clientThingDeleted = True
        }

setupServerQuery :: SS -> SqlPersistT IO ()
setupServerQuery ServerStore {..} = forM_ (M.toList serverStoreItems) $ \(i, e) -> void $ insertKey i e

clientGetStoreQuery :: SqlPersistT IO CS
clientGetStoreQuery = do
  clientStoreAdded <-
    M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingClientId !=. Nothing,
          ClientThingServerId ==. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  clientStoreSynced <-
    M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingServerId, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingClientId ==. Nothing,
          ClientThingServerId !=. Nothing,
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

clientMakeSyncRequestQuery :: SqlPersistT IO SReq
clientMakeSyncRequestQuery = do
  syncRequestAdded <-
    M.fromList . map (\(Entity _ ClientThing {..}) -> (fromJust clientThingClientId, ServerThing {serverThingNumber = clientThingNumber}))
      <$> selectList
        [ ClientThingClientId !=. Nothing,
          ClientThingServerId ==. Nothing,
          ClientThingDeleted ==. False
        ]
        []
  syncRequestSynced <-
    S.fromList . map (\(Entity _ ClientThing {..}) -> fromJust clientThingServerId)
      <$> selectList
        [ ClientThingClientId ==. Nothing,
          ClientThingServerId !=. Nothing,
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

serverGetStoreQuery :: SqlPersistT IO SS
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, st)) <$> selectList [] []

serverProcessSyncQuery :: SReq -> SqlPersistT IO SResp
serverProcessSyncQuery sr = do
  let serverSyncProcessorDeleteMany s = do
        deleteWhere [ServerThingId <-. S.toList s] -- FIXME this operator can crash
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced s = do
        aliases <-
          selectList [ServerThingId <-. S.toList s] [] -- FIXME this operator can crash
        let inSButNotInStore =
              s `S.difference` S.fromList (map entityKey aliases)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (\(Entity ti t) -> (ti, t))
          <$> selectList [ServerThingId /<-. S.toList s] [] -- FIXME this operator can crash
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
  let clientSyncProcessorSyncServerAdded m = forM_ (M.toList m) $ \(si, ServerThing {..}) ->
        insert_
          ( ClientThing
              { clientThingNumber = serverThingNumber,
                clientThingClientId = Nothing,
                clientThingServerId = Just si,
                clientThingDeleted = False
              }
          )
      clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(cid, sid) ->
        updateWhere [ClientThingClientId ==. Just cid] [ClientThingClientId =. Nothing, ClientThingServerId =. Just sid]
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid]
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [ClientThingServerId ==. Just sid, ClientThingDeleted ==. True]
      proc = ClientSyncProcessor {..}
  mergeSyncResponseCustom proc sr
