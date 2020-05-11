{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeless.CollectionSpec
  ( spec,
  )
where

import Control.Monad.State
import Data.GenValidity.Mergeless.Collection ()
import Data.GenValidity.UUID ()
import Data.Int (Int)
import Data.List
import qualified Data.Map.Strict as M
import Data.Mergeless.Collection
import Data.Ord
import qualified Data.Set as S
import Data.UUID
import GHC.Generics (Generic)
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  genValidSpec @(ClientStore Int Int)
  jsonSpecOnValid @(ClientStore Int Int)
  genValidSpec @(SyncRequest Int Int)
  jsonSpecOnValid @(SyncRequest Int Int)
  genValidSpec @(SyncResponse Int Int)
  jsonSpecOnValid @(SyncResponse Int Int)
  genValidSpec @(ServerStore Int Int)
  jsonSpecOnValid @(ServerStore Int Int)
  describe "emptyStore" $ it "is valid" $ shouldBeValid (emptyClientStore @Int @Int)
  describe "storeSize" $ do
    it "does not crash" $ producesValidsOnValids (storeSize @Int @Int)
    specify "adding an item makes the store bigger"
      $ forAllValid
      $ \store ->
        forAllValid $ \added -> do
          let size1 = storeSize (store :: ClientStore Int Int)
          let store' = addItemToClientStore added store
          let size2 = storeSize store'
          size2 `shouldBe` (size1 + 1)
    specify "deleting an unsynced item after adding it leaves the store with the original size"
      $ forAllValid
      $ \store ->
        forAllValid $ \added ->
          let size1 = storeSize (store :: ClientStore Int Int)
              store' = addItemToClientStore added store
           in case sortOn (Down . fst) $ M.toList (clientStoreAdded store') of
                [] -> expectationFailure "Expected a nonempty list"
                (i, _) : _ ->
                  let store'' = deleteUnsyncedFromClientStore i store'
                      size2 = storeSize store''
                   in size2 `shouldBe` size1
    specify "deleting a synced item after adding it leaves the store with the original size"
      $ forAllValid
      $ \store ->
        forAll (genValid `suchThat` (\uuid -> not $ M.member uuid $ clientStoreSynced store)) $ \uuid ->
          forAllValid $ \synced ->
            let size1 = storeSize (store :: ClientStore Int Int)
                store' = store {clientStoreSynced = M.insert uuid synced $ clientStoreSynced store}
                store'' = deleteSyncedFromClientStore uuid store'
                size2 = storeSize store''
             in size2 `shouldBe` size1
  describe "addItemToClientStore"
    $ it "produces valid stores"
    $ producesValidsOnValids2 (addItemToClientStore @Int @Int)
  describe "deleteUnsyncedFromClientStore"
    $ it "produces valid stores"
    $ producesValidsOnValids2 (deleteUnsyncedFromClientStore @Int @Int)
  describe "deleteSyncedFromClientStore"
    $ it "produces valid stores"
    $ producesValidsOnValids2 (deleteSyncedFromClientStore @Int @Int)
  describe "makeSyncRequest"
    $ it "produces valid sync requests"
    $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeSyncResponse" $ do
    it "produces valid sync stores" $ producesValidsOnValids2 (mergeSyncResponse @Int @Int)
    it "adds the single item that the server tells it to add to an empty client store"
      $ forAllValid
      $ \cid ->
        forAllValid $ \a ->
          forAllValid $ \u -> do
            let cstore1 = emptyClientStore {clientStoreAdded = M.singleton cid (a :: Int)}
                resp = emptySyncResponse {syncResponseClientAdded = M.singleton cid (u :: Int)}
                cstore2 = mergeSyncResponse cstore1 resp
            clientStoreSynced cstore2 `shouldBe` M.singleton u a
    it "deletes items that the server instructed to be deleted"
      $ forAllValid
      $ \cs ->
        forAllValid $ \sr -> do
          let cs' = mergeSyncResponse @Int @Int cs sr
          clientStoreDeleted cs'
            `shouldBe` (clientStoreDeleted cs `S.difference` syncResponseClientDeleted sr)
  describe "processServerSync"
    $ describe "deterministic UUIDs"
    $ serverSyncSpec @Int evalDM
    $ processServerSync genD

serverSyncSpec ::
  forall a i m.
  (Show i, Ord i, GenValid i, Show a, Ord a, GenValid a, MonadIO m) =>
  (forall r. m r -> IO r) ->
  (ServerStore i a -> SyncRequest i a -> m (SyncResponse i a, ServerStore i a)) ->
  Spec
serverSyncSpec eval func = do
  describe "Single client" $ do
    describe "Single-item" $ do
      it "succesfully downloads a single item from the server for an empty client"
        $ forAllValid
        $ \u ->
          forAllValid $ \i ->
            eval $ do
              let sstore1 = emptyServerStore {serverStoreItems = M.singleton u i}
              let cstore1 = emptyClientStore
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- func sstore1 req
              let cstore2 = mergeSyncResponse cstore1 resp
              liftIO $ do
                sstore2 `shouldBe` sstore1
                clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads a single item to the server for an empty server"
        $ forAllValid
        $ \c ->
          forAllValid $ \i ->
            eval $ do
              let cstore1 = emptyClientStore {clientStoreAdded = M.singleton c i}
              let sstore1 = emptyServerStore
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- func sstore1 req
              let cstore2 = mergeSyncResponse cstore1 resp
              liftIO $ do
                clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
                sort (M.elems (clientStoreSynced cstore2))
                  `shouldBe` sort (M.elems $ M.singleton c i)
    describe "Multi-item" $ do
      it "succesfully downloads everything from the server for an empty client"
        $ forAllValid
        $ \sstore1 ->
          eval $ do
            let cstore1 = emptyClientStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- func sstore1 req
            let cstore2 = mergeSyncResponse cstore1 resp
            liftIO $ do
              sstore2 `shouldBe` sstore1
              clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server"
        $ forAllValid
        $ \items ->
          eval $ do
            let cstore1 = emptyClientStore {clientStoreAdded = items}
            let sstore1 = emptyServerStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- func sstore1 req
            let cstore2 = mergeSyncResponse cstore1 resp
            liftIO $ do
              clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
              sort (M.elems (clientStoreSynced cstore2)) `shouldBe` sort (M.elems items)
      it "is idempotent with one client"
        $ forAllValid
        $ \cstore1 ->
          forAllValid $ \sstore1 ->
            eval $ do
              let req1 = makeSyncRequest cstore1
              (resp1, sstore2) <- func sstore1 req1
              let cstore2 = mergeSyncResponse cstore1 resp1
                  req2 = makeSyncRequest cstore2
              (resp2, sstore3) <- func sstore2 req2
              let cstore3 = mergeSyncResponse cstore2 resp2
              liftIO $ do
                cstore2 `shouldBe` cstore3
                sstore2 `shouldBe` sstore3
  describe "Multiple clients" $ do
    describe "Single-item" $ do
      it "successfully syncs an addition accross to a second client"
        $ forAllValid
        $ \i ->
          eval $ do
            let cAstore1 = emptyClientStore {clientStoreAdded = M.singleton (ClientId 0) i}
            -- Client B is empty
            let cBstore1 = emptyClientStore
            -- The server is empty
            let sstore1 = emptyServerStore
            -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
            -- The server processes sync request 1
            (resp1, sstore2) <- func sstore1 req1
            let addedItems = syncResponseClientAdded resp1
            case M.toList addedItems of
              [(ClientId 0, clientAdditionId)] -> do
                let items = M.singleton clientAdditionId i
                liftIO $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                -- Client A merges the response
                let cAstore2 = mergeSyncResponse cAstore1 resp1
                liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                -- The server processes sync request 2
                (resp2, sstore3) <- func sstore2 req2
                liftIO $ do
                  resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                  sstore3 `shouldBe` sstore2
                --  pPrint cBstore2
                -- Client B merges the response
                let cBstore2 = mergeSyncResponse cBstore1 resp2
                liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                -- Client A and Client B now have the same store
                liftIO $ cAstore2 `shouldBe` cBstore2
              _ -> liftIO $ expectationFailure "Should have found exactly one added item."
      it "succesfully syncs a deletion across to a second client"
        $ forAllValid
        $ \uuid ->
          forAllValid $ \i ->
            eval $ do
              let cAstore1 = emptyClientStore {clientStoreSynced = M.singleton uuid i}
              -- Client A has a synced item.
              -- Client B had synced that same item, but has since deleted it.
              let cBstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
              -- The server still has the undeleted item
              let sstore1 = ServerStore {serverStoreItems = M.singleton uuid i}
              -- Client B makes sync request 1
              let req1 = makeSyncRequest cBstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- func sstore1 req1
              liftIO $ do
                resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
                sstore2 `shouldBe` emptyServerStore
              -- Client B merges the response
              let cBstore2 = mergeSyncResponse cBstore1 resp1
              liftIO $ cBstore2 `shouldBe` emptyClientStore
              -- Client A makes sync request 2
              let req2 = makeSyncRequest cAstore1
              -- The server processes sync request 2
              (resp2, sstore3) <- func sstore2 req2
              liftIO $ do
                resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
                sstore3 `shouldBe` sstore2
              -- Client A merges the response
              let cAstore2 = mergeSyncResponse cAstore1 resp2
              liftIO $ cAstore2 `shouldBe` emptyClientStore
              -- Client A and Client B now have the same store
              liftIO $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion"
        $ forAllValid
        $ \uuid ->
          forAllValid $ \i ->
            eval $ do
              let cAstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
              -- Both client a and client b delete an item.
              let cBstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
              -- The server still has the undeleted item
              let sstore1 = ServerStore {serverStoreItems = M.singleton uuid i}
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- func sstore1 req1
              liftIO $ do
                resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
              -- Client A merges the response
              let cAstore2 = mergeSyncResponse cAstore1 resp1
              liftIO $ cAstore2 `shouldBe` emptyClientStore
              -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
              (resp2, sstore3) <- func sstore2 req2
              liftIO $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                sstore3 `shouldBe` sstore2
              -- Client B merges the response
              let cBstore2 = mergeSyncResponse cBstore1 resp2
              liftIO $ do
                cBstore2 `shouldBe` emptyClientStore
                -- Client A and Client B now have the same store
                cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      it
        "makes no change if the sync request reflects the same local state with an empty sync response"
        $ forAllValid
        $ \sis -> do
          let cs = ServerStore sis
          (sr, cs') <-
            eval
              $ func cs
              $ SyncRequest
                { syncRequestAdded = M.empty,
                  syncRequestSynced = M.keysSet sis,
                  syncRequestDeleted = S.empty
                }
          cs' `shouldBe` cs
          sr
            `shouldBe` SyncResponse
              { syncResponseClientAdded = M.empty,
                syncResponseClientDeleted = S.empty,
                syncResponseServerAdded = M.empty,
                syncResponseServerDeleted = S.empty
              }
      it "successfully syncs additions accross to a second client"
        $ forAllValid
        $ \is ->
          eval $ do
            let cAstore1 = emptyClientStore {clientStoreAdded = is}
            -- Client B is empty
            let cBstore1 = emptyClientStore
            -- The server is empty
            let sstore1 = emptyServerStore
            -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
            -- The server processes sync request 1
            (resp1, sstore2) <- func sstore1 req1
            -- Client A merges the response
            let cAstore2 = mergeSyncResponse cAstore1 resp1
            let items = clientStoreSynced cAstore2
            liftIO $ do
              clientStoreAdded cAstore2 `shouldBe` M.empty
              sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
            liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
            -- Client B makes sync request 2
            let req2 = makeSyncRequest cBstore1
            -- The server processes sync request 2
            (resp2, sstore3) <- func sstore2 req2
            liftIO $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            let cBstore2 = mergeSyncResponse cBstore1 resp2
            liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
            -- Client A and Client B now have the same store
            liftIO $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs deletions across to a second client"
        $ forAllValid
        $ \syncedItems ->
          eval $ do
            let itemIds = M.keysSet syncedItems
            let cAstore1 = emptyClientStore {clientStoreSynced = syncedItems}
            -- Client A has synced items
            -- Client B had synced the same items, but has since deleted them.
            let cBstore1 = emptyClientStore {clientStoreDeleted = itemIds}
            -- The server still has the undeleted item
            let sstore1 = ServerStore {serverStoreItems = syncedItems}
            -- Client B makes sync request 1
            let req1 = makeSyncRequest cBstore1
            -- The server processes sync request 1
            (resp1, sstore2) <- func sstore1 req1
            liftIO $ do
              resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = itemIds}
              sstore2 `shouldBe` emptyServerStore
            -- Client B merges the response
            let cBstore2 = mergeSyncResponse cBstore1 resp1
            liftIO $ cBstore2 `shouldBe` emptyClientStore
            -- Client A makes sync request 2
            let req2 = makeSyncRequest cAstore1
            -- The server processes sync request 2
            (resp2, sstore3) <- func sstore2 req2
            liftIO $ do
              resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = itemIds}
              sstore3 `shouldBe` sstore2
            -- Client A merges the response
            let cAstore2 = mergeSyncResponse cAstore1 resp2
            liftIO $ cAstore2 `shouldBe` emptyClientStore
            -- Client A and Client B now have the same store
            liftIO $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion"
        $ forAllValid
        $ \items ->
          eval $ do
            let cAstore1 = emptyClientStore {clientStoreDeleted = M.keysSet items}
            -- Both client a and client b delete their items.
            let cBstore1 = emptyClientStore {clientStoreDeleted = M.keysSet items}
            -- The server still has the undeleted items
            let sstore1 = ServerStore {serverStoreItems = items}
            -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
            -- The server processes sync request 1
            (resp1, sstore2) <- func sstore1 req1
            liftIO $ do
              resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
              sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                    -- Client A merges the response
            let cAstore2 = mergeSyncResponse cAstore1 resp1
            liftIO $ cAstore2 `shouldBe` emptyClientStore
            -- Client B makes sync request 2
            let req2 = makeSyncRequest cBstore1
            -- The server processes sync request 2
            (resp2, sstore3) <- func sstore2 req2
            liftIO $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            let cBstore2 = mergeSyncResponse cBstore1 resp2
            liftIO $ do
              cBstore2 `shouldBe` emptyClientStore
              -- Client A and Client B now have the same store
              cAstore2 `shouldBe` cBstore2
  describe "General properties" $ do
    it "produces valid results"
      $ forAllValid
      $ \cs ->
        forAllValid $ \sr -> do
          res <- eval $ func cs sr
          shouldBeValid res
    it "successfully syncs two clients using a central store"
      $ forAllValid
      $ \store1 ->
        eval $ do
          let central = ServerStore M.empty
          let store2 = emptyClientStore
          let sreq1 = makeSyncRequest store1
          (sresp1, central') <- func central sreq1
          let store1' = mergeSyncResponse store1 sresp1
          let sreq2 = makeSyncRequest store2
          (sresp2, central'') <- func central' sreq2
          let store2' = mergeSyncResponse store2 sresp2
          let sreq3 = makeSyncRequest store1'
          (sresp3, _) <- func central'' sreq3
          let store1'' = mergeSyncResponse store1' sresp3
          liftIO $ store1'' `shouldBe` store2'
    it "ensures that syncing is idempotent"
      $ forAllValid
      $ \central1 ->
        forAllValid $ \local1 ->
          eval $ do
            let sreq1 = makeSyncRequest local1
            (sresp1, central2) <- func central1 sreq1
            let local2 = mergeSyncResponse local1 sresp1
            let sreq2 = makeSyncRequest local2
            (sresp2, central3) <- func central2 sreq2
            let local3 = mergeSyncResponse local2 sresp2
            liftIO $ do
              local2 `shouldBe` local3
              central2 `shouldBe` central3

newtype D m a
  = D
      { unD :: StateT StdGen m a
      }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO)

-- evalD :: D Identity a -> a
-- evalD = runIdentity . evalDM
-- runD :: D Identity a -> StdGen -> (a, StdGen)
-- runD = runState . unD
evalDM :: Functor m => D m a -> m a
evalDM d = fst <$> runDM d (mkStdGen 42)

runDM :: D m a -> StdGen -> m (a, StdGen)
runDM = runStateT . unD

genD :: Monad m => D m UUID
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u
