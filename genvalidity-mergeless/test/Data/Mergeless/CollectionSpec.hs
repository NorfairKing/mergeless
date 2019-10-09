{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Mergeless.CollectionSpec
  ( spec
  ) where

import Data.Int (Int)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
  describe "storeSize" $ it "does not crash" $ producesValidsOnValids (storeSize @Int @Int)
  describe "addItemToClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (addItemToClientStore @Int @Int)
  describe "deleteUnsyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteUnsyncedFromClientStore @Int @Int)
  describe "deleteSyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteSyncedFromClientStore @Int @Int)
  describe "storeSize" $ do
    specify "deleting an unsynced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAllValid $ \added ->
          let size1 = storeSize (store :: ClientStore Int Int)
              store' = addItemToClientStore added store
           in case M.toList (clientStoreAdded store') of
                [(i, _)] ->
                  let store'' = deleteUnsyncedFromClientStore i store'
                      size2 = storeSize store''
                   in size2 `shouldBe` size1
                _ -> expectationFailure "Expected exactly one element."
    specify "deleting a synced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAllValid $ \(uuid, synced) ->
          let size1 = storeSize (store :: ClientStore Int Int)
              store' = store {clientStoreSynced = M.insert uuid synced $ clientStoreSynced store}
              store'' = deleteSyncedFromClientStore uuid store'
              size2 = storeSize store''
           in size2 `shouldBe` size1
  describe "makeSyncRequest" $
    it "produces valid sync requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeSyncResponse" $
    it "produces valid sync stores" $ producesValidsOnValids2 (mergeSyncResponse @Int @Int)
  describe "processServerSyncWith" $ do
    it
      "makes no change if the sync request reflects the same local state with an empty sync response" $
      forAllValid $ \synct ->
        forAllValid $ \sis -> do
          let cs = ServerStore sis
          let (sr, cs') =
                evalD $
                processServerSyncWith @UUID @Int genD synct cs $
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
    it "deletes the deleted items" $
      forAllValid $ \synct ->
        forAllValid $ \cs ->
          forAllValid $ \sreq -> do
            let (_, cs') = evalD $ processServerSyncWith @UUID @Int genD synct cs sreq
            syncRequestDeleted sreq `shouldSatisfy`
              (not . any (`S.member` (M.keysSet $ serverStoreItems cs')))
    it "returns the items that were added in the sync response" $
      forAllValid $ \synct ->
        forAllValid $ \cs ->
          forAllValid $ \sreq -> do
            let (sresp, _) = evalD $ processServerSyncWith @UUID @Int genD synct cs sreq
            M.keysSet (syncResponseClientAdded sresp) `shouldBe` M.keysSet (syncRequestAdded sreq)
    it "returns the single added item" $
      forAllValid $ \synct ->
        forAllValid $ \cs ->
          forAllValid $ \ai -> do
            let (sresp, _) =
                  evalD $
                  processServerSyncWith
                    @UUID
                    @Int
                    genD
                    synct
                    cs
                    SyncRequest
                      { syncRequestAdded = M.singleton (ClientId 0) ai
                      , syncRequestSynced = S.empty
                      , syncRequestDeleted = S.empty
                      }
            M.keysSet (syncResponseClientAdded sresp) `shouldBe` S.singleton (ClientId 0)
    it "adds the items that were added" $
      forAllValid $ \synct ->
        forAllValid $ \cs ->
          forAllValid $ \sreq -> do
            let (_, cs') = evalD $ processServerSyncWith @UUID @Int genD synct cs sreq
            S.fromList (M.elems (M.map addedValue (syncRequestAdded sreq))) `shouldSatisfy`
              (`S.isSubsetOf` (S.fromList $ M.elems $ M.map syncedValue $ serverStoreItems cs'))
    it
      "returns the single remotely added item if the sync request is empty and the central store has one item" $
      forAllValid $ \synct ->
        forAllValid $ \(uuid, si) -> do
          let (sresp, _) =
                evalD $
                processServerSyncWith
                  @UUID
                  @Int
                  genD
                  synct
                  (ServerStore $ M.singleton uuid si)
                  SyncRequest
                    { syncRequestAdded = M.empty
                    , syncRequestSynced = S.empty
                    , syncRequestDeleted = S.empty
                    }
          syncResponseServerAdded sresp `shouldBe` M.singleton uuid si
    it "returns all remotely added items when no items are locally added or deleted" $
      forAllValid $ \synct ->
        forAllValid $ \ss ->
          forAllValid $ \sis -> do
            let (sresp, _) =
                  evalD $
                  processServerSyncWith
                    @UUID
                    @Int
                    genD
                    synct
                    ss
                    SyncRequest
                      { syncRequestAdded = M.empty
                      , syncRequestSynced = sis
                      , syncRequestDeleted = S.empty
                      }
            syncResponseServerAdded sresp `shouldBe`
              (serverStoreItems ss `M.difference` (M.fromSet (const ()) sis))
    it "returns all remotely added items when no items are locally deleted " $
      forAllValid $ \synct ->
        forAllValid $ \ss ->
          forAllValid $ \sis ->
            forAllValid $ \ais -> do
              let (sresp, _) =
                    evalD $
                    processServerSyncWith
                      @UUID
                      @Int
                      genD
                      synct
                      ss
                      SyncRequest
                        { syncRequestAdded = ais
                        , syncRequestSynced = sis
                        , syncRequestDeleted = S.empty
                        }
              syncResponseServerAdded sresp `shouldBe`
                (serverStoreItems ss `M.difference` (M.fromSet (const ()) sis))
    it "returns all remotely added items that weren't deleted when no items are locally added " $
      forAllValid $ \synct ->
        forAllValid $ \ss ->
          forAllValid $ \sis ->
            forAllValid $ \dis -> do
              let (sresp, _) =
                    evalD $
                    processServerSyncWith
                      @UUID
                      @Int
                      genD
                      synct
                      ss
                      SyncRequest
                        { syncRequestAdded = M.empty
                        , syncRequestSynced = sis
                        , syncRequestDeleted = dis
                        }
              syncResponseServerAdded sresp `shouldBe`
                (serverStoreItems ss `M.difference` M.fromSet (const ()) sis)
    it "returns all remotely added items that weren't deleted" $
      forAllValid $ \synct ->
        forAllValid $ \ss ->
          forAllValid $ \sreq -> do
            let (sresp, _) = evalD $ processServerSyncWith @UUID @Int genD synct ss sreq
            syncResponseServerAdded sresp `shouldBe`
              (serverStoreItems ss `M.difference`
               M.fromSet (const ()) (syncRequestDeleted sreq `S.union` syncRequestSynced sreq))
    it "successfully syncs two clients using a central store when using incrementing words" $
      forAllValid $ \store1 ->
        forAllValid $ \(synct1, synct2, synct3) -> do
          let (s1, s2) =
                evalI $ do
                  let central = ServerStore M.empty
                  let store2 = emptyClientStore
                  let sreq1 = makeSyncRequest @Word @Int store1
                  (sresp1, central') <- processServerSyncWith genI synct1 central sreq1
                  let store1' = mergeSyncResponse store1 sresp1
                  let sreq2 = makeSyncRequest store2
                  (sresp2, central'') <- processServerSyncWith genI synct2 central' sreq2
                  let store2' = mergeSyncResponse store2 sresp2
                  let sreq3 = makeSyncRequest store1'
                  (sresp3, _) <- processServerSyncWith genI synct3 central'' sreq3
                  let store1'' = mergeSyncResponse store1' sresp3
                  pure (store1'', store2')
          s1 `shouldBe` s2
    it "successfully syncs two clients using a central store when using deterministic UUIDs" $
      forAllValid $ \store1 ->
        forAllValid $ \(synct1, synct2, synct3) -> do
          let (s1, s2) =
                evalD $ do
                  let central = ServerStore M.empty
                  let store2 = emptyClientStore
                  let sreq1 = makeSyncRequest @UUID @Int store1
                  (sresp1, central') <- processServerSyncWith genD synct1 central sreq1
                  let store1' = mergeSyncResponse store1 sresp1
                  let sreq2 = makeSyncRequest store2
                  (sresp2, central'') <- processServerSyncWith genD synct2 central' sreq2
                  let store2' = mergeSyncResponse store2 sresp2
                  let sreq3 = makeSyncRequest store1'
                  (sresp3, _) <- processServerSyncWith genD synct3 central'' sreq3
                  let store1'' = mergeSyncResponse store1' sresp3
                  pure (store1'', store2')
          s1 `shouldBe` s2
    it
      "produces valid results when building up a central store from nothing using incrementing words" $
      forAll (genListOf $ (,) <$> genUnsyncedStore <*> genValid) $ \tups ->
        shouldBeValid $
        evalI $ do
          let initServerStore = ServerStore M.empty
          let go cs (store, synct) = do
                let sreq = makeSyncRequest @Word @Int store
                (_, central') <- processServerSyncWith genI synct cs sreq
                pure central'
          foldM go initServerStore (tups :: [(ClientStore Word Int, UTCTime)])
    it
      "produces valid results when building up a central store from nothing using deterministic UUIDs" $
      forAllValid $ \tups ->
        shouldBeValid $
        evalD $ do
          let initServerStore = ServerStore M.empty
          let go cs (store, synct) = do
                let sreq = makeSyncRequest @UUID @Int store
                (_, central') <- processServerSyncWith genD synct cs sreq
                pure central'
          foldM go initServerStore (tups :: [(ClientStore UUID Int, UTCTime)])
    it "produces valid results when using incrementing words starting from an empty store" $
      forAllValid $ \synct ->
        forAll (makeSyncRequest <$> genUnsyncedStore) $ \sreq -> do
          let r@(sresp, cs') =
                evalI $ processServerSyncWith @Word @Int genI synct emptyServerStore sreq
          case prettyValidate r of
            Left err ->
              expectationFailure $
              unlines
                ["Inputs:", ppShow synct, ppShow sreq, "Outputs:", ppShow sresp, ppShow cs', err]
            Right _ -> pure ()
    it "produces valid results when using determinisitic UUIDs" $
      producesValidsOnValids3 $ \synct cs sr ->
        evalD $ processServerSyncWith @UUID @Int genD synct cs sr
    it "makes syncing idempotent with incrementing words" $
      forAllValid $ \synct1 ->
        forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
          forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
              let d1 = 0
              let sreq1 = makeSyncRequest @Word @Int local1
              let ((sresp1, central2), d2) =
                    runI (processServerSyncWith genI synct1 central1 sreq1) d1
              let local2 = mergeSyncResponse local1 sresp1
              let sreq2 = makeSyncRequest local2
              let ((sresp2, central3), _) =
                    runI (processServerSyncWith genI synct2 central2 sreq2) d2
              let local3 = mergeSyncResponse local2 sresp2
              local2 `shouldBe` local3
              central2 `shouldBe` central3
    it "makes syncing idempotent with deterministic UUIDs" $
      forAllValid $ \synct1 ->
        forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
          forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
              let d1 = mkStdGen 42
              let sreq1 = makeSyncRequest @UUID @Int local1
              let ((sresp1, central2), d2) =
                    runD (processServerSyncWith genD synct1 central1 sreq1) d1
              let local2 = mergeSyncResponse local1 sresp1
              let sreq2 = makeSyncRequest local2
              let ((sresp2, central3), _) =
                    runD (processServerSyncWith genD synct2 central2 sreq2) d2
              let local3 = mergeSyncResponse local2 sresp2
              local2 `shouldBe` local3
              central2 `shouldBe` central3
    it "makes syncing idempotent with random UUIDs" $
      forAllValid $ \synct1 ->
        forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
          forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
              let sreq1 = makeSyncRequest @UUID @Int local1
              (sresp1, central2) <- processServerSyncWith UUID.nextRandom synct1 central1 sreq1
              let local2 = mergeSyncResponse local1 sresp1
              let sreq2 = makeSyncRequest local2
              (sresp2, central3) <- processServerSyncWith UUID.nextRandom synct2 central2 sreq2
              let local3 = mergeSyncResponse local2 sresp2
              local2 `shouldBe` local3
              central2 `shouldBe` central3
  describe "processSync" $
    it "makes syncing idempotent when using random UUIDs" $
    forAllValid $ \central1 ->
      forAllValid $ \local1 -> do
        let sreq1 = makeSyncRequest @UUID @Int local1
        (sresp1, central2) <- processServerSync UUID.nextRandom central1 sreq1
        let local2 = mergeSyncResponse local1 sresp1
        let sreq2 = makeSyncRequest local2
        (sresp2, central3) <- processServerSync UUID.nextRandom central2 sreq2
        let local3 = mergeSyncResponse local2 sresp2
        local2 `shouldBe` local3
        central2 `shouldBe` central3

newtype D a =
  D
    { unD :: State StdGen a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen)

evalD :: D a -> a
evalD d = fst $ runD d $ mkStdGen 42

runD :: D a -> StdGen -> (a, StdGen)
runD = runState . unD

genD :: D UUID
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
