{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Mergeless.Persistent
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $
  oneClientSpec $ do
    describe "sanity" $ do
      describe "setupClient & clientGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \cstore -> runTest te $ do
          setupClient cstore
          cstore' <- clientGetStore
          liftIO $ cstore' `shouldBe` cstore
      describe "setupServer & serverGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \sstore -> runTest te $ do
          setupServer sstore
          sstore' <- serverGetStore
          liftIO $ sstore' `shouldBe` sstore
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

type CS = ClientStore ClientThingId ServerThingId Thing

type SReq = SyncRequest ClientThingId ServerThingId Thing

type SS = ServerStore ServerThingId Thing

type SResp = SyncResponse ClientThingId ServerThingId Thing

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

setupUnsyncedClient :: [Thing] -> T ()
setupUnsyncedClient = runClientDB . setupUnsyncedClientQuery makeUnsyncedClientThing

setupClient :: CS -> T ()
setupClient = runClientDB . setupClientThingQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerThingQuery

clientGetStore :: T CS
clientGetStore = runClientDB clientGetStoreThingQuery

clientMakeSyncRequest :: T SReq
clientMakeSyncRequest = runClientDB clientMakeSyncRequestThingQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreThingQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncThingQuery

clientMergeSyncResponse :: SResp -> T ()
clientMergeSyncResponse = runClientDB . clientMergeSyncResponseThingQuery

data TestEnv = TestEnv
  { testEnvClientPool :: !ConnectionPool,
    testEnvServerPool :: !ConnectionPool
  }

oneClientSpec :: SpecWith TestEnv -> Spec
oneClientSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool -> withClientPool $ \clientPool -> do
    let tenv = TestEnv {testEnvClientPool = clientPool, testEnvServerPool = serverPool}
    liftIO $ func tenv
