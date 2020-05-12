{-# LANGUAGE RankNTypes #-}
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
import Data.Mergeless
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ oneClientSpec $ do
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
setupUnsyncedClient = runClientDB . setupUnsyncedClientQuery

type CS = ClientStore ClientId ServerThingId ServerThing

type SReq = SyncRequest ClientId ServerThingId ServerThing

type SS = ServerStore ServerThingId ServerThing

type SResp = SyncResponse ClientId ServerThingId ServerThing

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

data TestEnv
  = TestEnv
      { testEnvClientPool :: ConnectionPool,
        testEnvServerPool :: ConnectionPool
      }

oneClientSpec :: SpecWith TestEnv -> Spec
oneClientSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool -> withClientPool 1 $ \clientPool -> do
    let tenv = TestEnv {testEnvClientPool = clientPool, testEnvServerPool = serverPool}
    liftIO $ func tenv
