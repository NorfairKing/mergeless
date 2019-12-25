{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeless.CollectionSpec
  ( spec
  ) where

import Data.Functor.Identity
import Data.Int (Int)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless.Collection
import Data.GenValidity.UUID ()
import Data.Mergeless.Collection
import Data.Mergeless.Item
import Data.UUID

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  eqSpecOnValid @(ClientStore Int Int)
  ordSpecOnValid @(ClientStore Int Int)
  genValidSpec @(ClientStore Int Int)
  jsonSpecOnValid @(ClientStore Int Int)
  eqSpecOnValid @(SyncRequest Int Int)
  ordSpecOnValid @(SyncRequest Int Int)
  genValidSpec @(SyncRequest Int Int)
  jsonSpecOnValid @(SyncRequest Int Int)
  eqSpecOnValid @(SyncResponse Int Int)
  ordSpecOnValid @(SyncResponse Int Int)
  genValidSpec @(SyncResponse Int Int)
  jsonSpecOnValid @(SyncResponse Int Int)
  eqSpecOnValid @(ServerStore Int Int)
  ordSpecOnValid @(ServerStore Int Int)
  genValidSpec @(ServerStore Int Int)
  jsonSpecOnValid @(ServerStore Int Int)
  describe "emptyStore" $ it "is valid" $ shouldBeValid (emptyClientStore @Int @Int)
  describe "storeSize" $ do
    it "does not crash" $ producesValidsOnValids (storeSize @Int @Int)
    specify "deleting an unsynced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAllValid $ \added ->
          let size1 = storeSize (store :: ClientStore Int Int)
              store' = addItemToClientStore added store
           in case sortOn (Down . fst) $ M.toList (clientStoreAdded store') of
                [] -> expectationFailure "Expected a nonempty list"
                (i, _):_ ->
                  let store'' = deleteUnsyncedFromClientStore i store'
                      size2 = storeSize store''
                   in size2 `shouldBe` size1
    specify "deleting a synced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAll (genValid `suchThat` (\uuid -> not $ M.member uuid $ clientStoreSynced store)) $ \uuid ->
          forAllValid $ \synced ->
            let size1 = storeSize (store :: ClientStore Int Int)
                store' = store {clientStoreSynced = M.insert uuid synced $ clientStoreSynced store}
                store'' = deleteSyncedFromClientStore uuid store'
                size2 = storeSize store''
             in size2 `shouldBe` size1
  describe "addItemToClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (addItemToClientStore @Int @Int)
  describe "deleteUnsyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteUnsyncedFromClientStore @Int @Int)
  describe "deleteSyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteSyncedFromClientStore @Int @Int)
  describe "makeSyncRequest" $
    it "produces valid sync requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeSyncResponse" $ do
    it "produces valid sync stores" $ producesValidsOnValids2 (mergeSyncResponse @Int @Int)
    it "deletes items that the server instructed to be deleted" $
      forAllValid $ \cs ->
        forAllValid $ \sr ->
          let cs' = mergeSyncResponse @Int @Int cs sr
           in clientStoreDeleted cs' `shouldBe`
              (clientStoreDeleted cs `S.difference` syncResponseClientDeleted sr)
  describe "processServerSyncWith" $ do
    describe "deterministic UUIDs" $ serverSyncSpec @Int evalD $ processServerSyncWith genD

serverSyncSpec ::
     forall a i m. (Show i, Ord i, GenValid i, Show a, Ord a, GenValid a, Monad m)
  => (forall r. m r -> r)
  -> (UTCTime -> ServerStore i a -> SyncRequest i a -> m (SyncResponse i a, ServerStore i a))
  -> Spec
serverSyncSpec eval func = do
  describe "Single client" $
    describe "Multi-item" $ do
      it "succesfully downloads everything from the server for an empty client" $
        forAllValid $ \sstore1 ->
          evalDM $ do
            let cstore1 = emptyClientStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- processServerSync @UUID @Int genD sstore1 req
            let cstore2 = mergeSyncResponse cstore1 resp
            lift $ do
              sstore2 `shouldBe` sstore1
              clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server" $
        forAllValid $ \items ->
          evalDM $ do
            let cstore1 = emptyClientStore {clientStoreAdded = items}
            let sstore1 = emptyServerStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- processServerSync @UUID @Int genD sstore1 req
            let cstore2 = mergeSyncResponse cstore1 resp
            lift $ do
              sort (M.elems (M.map syncedValue (clientStoreSynced cstore2))) `shouldBe`
                sort (M.elems $ M.map addedValue items)
              clientStoreSynced cstore2 `shouldBe` serverStoreItems sstore2
      it "is idempotent with one client" $
        forAllValid $ \cstore1 ->
          forAllValid $ \sstore1 ->
            evalDM $ do
              let req1 = makeSyncRequest cstore1
              (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
              let cstore2 = mergeSyncResponse cstore1 resp1
                  req2 = makeSyncRequest cstore2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              let cstore3 = mergeSyncResponse cstore2 resp2
              lift $ do
                cstore2 `shouldBe` cstore3
                sstore2 `shouldBe` sstore3
  describe "Multiple clients" $ do
    describe "Single-item" $ do
      it "successfully syncs an addition accross to a second client" $
        forAllValid $ \i ->
          evalDM $ do
            let cAstore1 = emptyClientStore {clientStoreAdded = M.singleton (ClientId 0) i}
              -- Client B is empty
            let cBstore1 = emptyClientStore
              -- The server is empty
            let sstore1 = emptyServerStore
              -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
            (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
            let addedItems = syncResponseClientAdded resp1
            case M.toList addedItems of
              [(ClientId 0, ClientAddition {..})] -> do
                let items = M.singleton clientAdditionId (addedToSynced clientAdditionTime i)
                lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                  -- Client A merges the response
                let cAstore2 = mergeSyncResponse cAstore1 resp1
                lift $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                  -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                  sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                let cBstore2 = mergeSyncResponse cBstore1 resp2
                lift $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                  -- Client A and Client B now have the same store
                lift $ cAstore2 `shouldBe` cBstore2
              _ -> lift $ expectationFailure "Should have found exactly one added item."
      it "succesfully syncs a deletion across to a second client" $
        forAllValid $ \uuid ->
          forAllValid $ \time1 ->
            forAllValid $ \i ->
              evalDM $ do
                let cAstore1 =
                      emptyClientStore
                        {clientStoreSynced = M.singleton uuid (addedToSynced time1 i)}
                  -- Client A has a synced item.
                  -- Client B had synced that same item, but has since deleted it.
                let cBstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
                  -- The server still has the undeleted item
                let sstore1 =
                      ServerStore {serverStoreItems = M.singleton uuid (addedToSynced time1 i)}
                  -- Client B makes sync request 1
                let req1 = makeSyncRequest cBstore1
                  -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                lift $ do
                  resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
                  sstore2 `shouldBe` emptyServerStore
                  -- Client B merges the response
                let cBstore2 = mergeSyncResponse @UUID @Int cBstore1 resp1
                lift $ cBstore2 `shouldBe` emptyClientStore
                  -- Client A makes sync request 2
                let req2 = makeSyncRequest cAstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
                  sstore3 `shouldBe` sstore2
                  -- Client A merges the response
                let cAstore2 = mergeSyncResponse cAstore1 resp2
                lift $ cAstore2 `shouldBe` emptyClientStore
                  -- Client A and Client B now have the same store
                lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $ \uuid ->
          forAllValid $ \time1 ->
            forAllValid $ \i ->
              evalDM $ do
                let cAstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
                  -- Both client a and client b delete an item.
                let cBstore1 = emptyClientStore {clientStoreDeleted = S.singleton uuid}
                  -- The server still has the undeleted item
                let sstore1 =
                      ServerStore {serverStoreItems = M.singleton uuid (addedToSynced time1 i)}
                  -- Client A makes sync request 1
                let req1 = makeSyncRequest cAstore1
                  -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
                lift $ do
                  resp1 `shouldBe`
                    (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                  sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
                  -- Client A merges the response
                let cAstore2 = mergeSyncResponse cAstore1 resp1
                lift $ cAstore2 `shouldBe` emptyClientStore
                  -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe`
                    (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                  sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                let cBstore2 = mergeSyncResponse cBstore1 resp2
                lift $ do
                  cBstore2 `shouldBe` emptyClientStore
                    -- Client A and Client B now have the same store
                  cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      it
        "makes no change if the sync request reflects the same local state with an empty sync response" $
        forAllValid $ \synct ->
          forAllValid $ \sis -> do
            let cs = ServerStore sis
            let (sr, cs') =
                  eval $
                  func synct cs $
                  SyncRequest
                    { syncRequestAdded = M.empty
                    , syncRequestSynced = M.keysSet sis
                    , syncRequestDeleted = S.empty
                    }
            cs' `shouldBe` cs
            sr `shouldBe`
              SyncResponse
                { syncResponseClientAdded = M.empty
                , syncResponseClientDeleted = S.empty
                , syncResponseServerAdded = M.empty
                , syncResponseServerDeleted = S.empty
                }
      it "successfully syncs additions accross to a second client" $
        forAllValid $ \is ->
          evalDM $ do
            let cAstore1 = emptyClientStore {clientStoreAdded = is}
              -- Client B is empty
            let cBstore1 = emptyClientStore
              -- The server is empty
            let sstore1 = emptyServerStore
              -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
            (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
              -- Client A merges the response
            let cAstore2 = mergeSyncResponse cAstore1 resp1
            let items = clientStoreSynced cAstore2
            lift $ do
              clientStoreAdded cAstore2 `shouldBe` M.empty
              sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
            lift $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              -- Client B makes sync request 2
            let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
            (resp2, sstore3) <- processServerSync genD sstore2 req2
            lift $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
              -- Client B merges the response
            let cBstore2 = mergeSyncResponse cBstore1 resp2
            lift $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
              -- Client A and Client B now have the same store
            lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs deletions across to a second client" $
        forAllValid $ \items ->
          forAllValid $ \time1 ->
            evalDM $ do
              let syncedItems = M.map (addedToSynced time1) items
                  itemIds = M.keysSet items
              let cAstore1 = emptyClientStore {clientStoreSynced = syncedItems}
                -- Client A has synced items
                -- Client B had synced the same items, but has since deleted them.
              let cBstore1 = emptyClientStore {clientStoreDeleted = itemIds}
                -- The server still has the undeleted item
              let sstore1 = ServerStore {serverStoreItems = syncedItems}
                -- Client B makes sync request 1
              let req1 = makeSyncRequest cBstore1
                -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
              lift $ do
                resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = itemIds}
                sstore2 `shouldBe` emptyServerStore
                -- Client B merges the response
              let cBstore2 = mergeSyncResponse cBstore1 resp1
              lift $ cBstore2 `shouldBe` emptyClientStore
                -- Client A makes sync request 2
              let req2 = makeSyncRequest cAstore1
                -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = itemIds}
                sstore3 `shouldBe` sstore2
                -- Client A merges the response
              let cAstore2 = mergeSyncResponse cAstore1 resp2
              lift $ cAstore2 `shouldBe` emptyClientStore
                -- Client A and Client B now have the same store
              lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $ \items ->
          forAllValid $ \time1 ->
            evalDM $ do
              let cAstore1 = emptyClientStore {clientStoreDeleted = M.keysSet items}
                -- Both client a and client b delete their items.
              let cBstore1 = emptyClientStore {clientStoreDeleted = M.keysSet items}
                -- The server still has the undeleted items
              let sstore1 = ServerStore {serverStoreItems = M.map (addedToSynced time1) items}
                -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
                -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync @UUID @Int genD sstore1 req1
              lift $ do
                resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                -- Client A merges the response
              let cAstore2 = mergeSyncResponse cAstore1 resp1
              lift $ cAstore2 `shouldBe` emptyClientStore
                -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
                -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                sstore3 `shouldBe` sstore2
                -- Client B merges the response
              let cBstore2 = mergeSyncResponse cBstore1 resp2
              lift $ do
                cBstore2 `shouldBe` emptyClientStore
                  -- Client A and Client B now have the same store
                cAstore2 `shouldBe` cBstore2
  describe "General properties" $ do
    it "produces valid results" $ producesValidsOnValids3 $ \synct cs sr -> eval $ func synct cs sr
    it "successfully syncs two clients using a central store" $
      forAllValid $ \store1 ->
        forAllValid $ \(synct1, synct2, synct3) -> do
          let (s1, s2) =
                eval $ do
                  let central = ServerStore M.empty
                  let store2 = emptyClientStore
                  let sreq1 = makeSyncRequest store1
                  (sresp1, central') <- func synct1 central sreq1
                  let store1' = mergeSyncResponse store1 sresp1
                  let sreq2 = makeSyncRequest store2
                  (sresp2, central'') <- func synct2 central' sreq2
                  let store2' = mergeSyncResponse store2 sresp2
                  let sreq3 = makeSyncRequest store1'
                  (sresp3, _) <- func synct3 central'' sreq3
                  let store1'' = mergeSyncResponse store1' sresp3
                  pure (store1'', store2')
          s1 `shouldBe` s2
    it "ensures that syncing is idempotent" $
      forAllValid $ \synct1 ->
        forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
          forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
              let ((local2, local3), (central2, central3)) =
                    eval $ do
                      let sreq1 = makeSyncRequest local1
                      (sresp1, central2) <- func synct1 central1 sreq1
                      let local2 = mergeSyncResponse local1 sresp1
                      let sreq2 = makeSyncRequest local2
                      (sresp2, central3) <- func synct2 central2 sreq2
                      let local3 = mergeSyncResponse local2 sresp2
                      pure ((local2, local3), (central2, central3))
              local2 `shouldBe` local3
              central2 `shouldBe` central3

newtype D m a =
  D
    { unD :: StateT StdGen m a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO)

evalD :: D Identity a -> a
evalD = runIdentity . evalDM

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

newtype I a =
  I
    { unI :: State Word a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState Word)

evalI :: I a -> a
evalI i = fst $ runI i 0

runI :: I a -> Word -> (a, Word)
runI = runState . unI

genI :: I Word
genI = do
  i <- get
  modify succ
  pure i

diffSet :: Ord i => Map i a -> Set i -> Map i a
diffSet m s = m `M.difference` toMap s

toMap :: Set i -> Map i ()
toMap = M.fromSet (const ())
