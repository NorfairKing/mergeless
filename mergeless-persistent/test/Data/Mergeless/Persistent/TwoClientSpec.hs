{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeless.Persistent.TwoClientSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.GenValidity.Mergeless
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Mergeless
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ twoClientsSpec $ do
  describe "Single item" $ do
    pure ()
  describe "Multiple items" $ do
    it "successfully syncs an addition accross to a second client" $
      \te ->
        forAllValid $ \st -> runTest te $ do
          setupUnsyncedClient A [st]
          setupUnsyncedClient B []
          setupServer emptyServerStore
          req1 <- clientMakeSyncRequest A
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          case M.toList (syncResponseClientAdded resp1) of
            [(ClientId 0, clientAdditionId)] -> do
              let items = M.singleton clientAdditionId st :: Map ServerThingId ServerThing
              liftIO $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
              clientMergeSyncResponse A resp1
              cAstore2 <- clientGetStore A
              liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              req2 <- clientMakeSyncRequest B
              resp2 <- serverProcessSync req2
              sstore3 <- serverGetStore
              liftIO $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                sstore3 `shouldBe` sstore2
              clientMergeSyncResponse B resp2
              cBstore2 <- clientGetStore B
              liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              liftIO $ cAstore2 `shouldBe` cBstore2
            _ -> liftIO $ expectationFailure "Should have found exactly one added item."
    it "succesfully syncs a deletion across to a second client" $
      \te -> forAllValid $ \uuid ->
        forAllValid $ \i ->
          runTest te $ do
            setupClient A $ emptyClientStore {clientStoreSynced = M.singleton uuid i}
            -- Client A has a synced item.
            -- Client B had synced that same item, but has since deleted it.
            setupClient B $ emptyClientStore {clientStoreDeleted = S.singleton uuid}
            -- The server still has the undeleted item
            setupServer $ ServerStore {serverStoreItems = M.singleton uuid i}
            -- Client B makes sync request 1
            req1 <- clientMakeSyncRequest B
            -- The server processes sync request 1
            resp1 <- serverProcessSync req1
            sstore2 <- serverGetStore
            liftIO $ do
              resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
              sstore2 `shouldBe` emptyServerStore
            -- Client B merges the response
            clientMergeSyncResponse B resp1
            cBstore2 <- clientGetStore B
            liftIO $ cBstore2 `shouldBe` emptyClientStore
            -- Client A makes sync request 2
            req2 <- clientMakeSyncRequest A
            -- The server processes sync request 2
            resp2 <- serverProcessSync req2
            sstore3 <- serverGetStore
            liftIO $ do
              resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
              sstore3 `shouldBe` sstore2
            -- Client A merges the response
            clientMergeSyncResponse A resp2
            cAstore2 <- clientGetStore A
            liftIO $ cAstore2 `shouldBe` emptyClientStore
            -- Client A and Client B now have the same store
            liftIO $ cAstore2 `shouldBe` cBstore2
    it "does not run into a conflict if two clients both try to sync a deletion" $
      \te -> forAllValid $ \uuid ->
        forAllValid $ \i ->
          runTest te $ do
            setupClient A $ emptyClientStore {clientStoreDeleted = S.singleton uuid}
            -- Both client a and client b delete an item.
            setupClient B $ emptyClientStore {clientStoreDeleted = S.singleton uuid}
            -- The server still has the undeleted item
            setupServer $ ServerStore {serverStoreItems = M.singleton uuid i}
            -- Client A makes sync request 1
            req1 <- clientMakeSyncRequest A
            -- The server processes sync request 1
            resp1 <- serverProcessSync req1
            sstore2 <- serverGetStore
            liftIO $ do
              resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
              sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
            -- Client A merges the response
            clientMergeSyncResponse A resp1
            cAstore2 <- clientGetStore A
            liftIO $ cAstore2 `shouldBe` emptyClientStore
            -- Client B makes sync request 2
            req2 <- clientMakeSyncRequest B
            -- The server processes sync request 2
            resp2 <- serverProcessSync req2
            sstore3 <- serverGetStore
            liftIO $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            clientMergeSyncResponse B resp2
            cBstore2 <- clientGetStore B
            liftIO $ do
              cBstore2 `shouldBe` emptyClientStore
              -- Client A and Client B now have the same store
              cAstore2 `shouldBe` cBstore2

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: Client -> SqlPersistT IO a -> T a
runClientDB num func = do
  pool <- asks $ case num of
    A -> testEnvClient1Pool
    B -> testEnvClient2Pool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

setupUnsyncedClient :: Client -> [ServerThing] -> T ()
setupUnsyncedClient n =
  runClientDB n . setupUnsyncedClientQuery

type CS = ClientStore ServerThingId ServerThing

type SReq = SyncRequest ServerThingId ServerThing

type SS = ServerStore ServerThingId ServerThing

type SResp = SyncResponse ServerThingId ServerThing

sync :: Client -> T (CS, SS, SS, CS)
sync n = do
  cstore1 <- clientGetStore n
  req <- clientMakeSyncRequest n
  sstore1 <- serverGetStore
  resp <- serverProcessSync req
  sstore2 <- serverGetStore
  clientMergeSyncResponse n resp
  cstore2 <- clientGetStore n
  pure (cstore1, sstore1, sstore2, cstore2)

setupClient :: Client -> CS -> T ()
setupClient n = runClientDB n . setupClientQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerQuery

clientGetStore :: Client -> T CS
clientGetStore n = runClientDB n clientGetStoreQuery

clientMakeSyncRequest :: Client -> T SReq
clientMakeSyncRequest n = runClientDB n clientMakeSyncRequestQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncQuery

clientMergeSyncResponse :: Client -> SResp -> T ()
clientMergeSyncResponse n = runClientDB n . clientMergeSyncResponseQuery

data Client = A | B
  deriving (Show, Eq)

data TestEnv
  = TestEnv
      { testEnvServerPool :: ConnectionPool,
        testEnvClient1Pool :: ConnectionPool,
        testEnvClient2Pool :: ConnectionPool
      }

twoClientsSpec :: SpecWith TestEnv -> Spec
twoClientsSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool ->
    withClientPool 1 $ \client1Pool ->
      withClientPool 2 $ \client2Pool -> do
        let tenv =
              TestEnv
                { testEnvServerPool = serverPool,
                  testEnvClient1Pool = client1Pool,
                  testEnvClient2Pool = client2Pool
                }
        liftIO $ func tenv
