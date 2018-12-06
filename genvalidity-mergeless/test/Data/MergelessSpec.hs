{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergelessSpec
    ( spec
    ) where

import Data.Int (Int)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless ()
import Data.GenValidity.UUID.Typed ()
import Data.Mergeless
import Data.UUID.Typed

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
    eqSpecOnValid @(Store Rational Rational)
    ordSpecOnValid @(Store Rational Rational)
    genValiditySpec @(Store Rational Rational)
    jsonSpecOnValid @(Store Rational Rational)
    eqSpecOnValid @(StoreItem Rational Rational)
    ordSpecOnValid @(StoreItem Rational Rational)
    genValiditySpec @(StoreItem Rational Rational)
    jsonSpecOnValid @(StoreItem Rational Rational)
    eqSpecOnValid @(Added Rational)
    ordSpecOnValid @(Added Rational)
    genValiditySpec @(Added Rational)
    jsonSpecOnValid @(Added Rational)
    eqSpecOnValid @(Synced Rational Rational)
    ordSpecOnValid @(Synced Rational Rational)
    genValiditySpec @(Synced Rational Rational)
    jsonSpecOnValid @(Synced Rational Rational)
    eqSpecOnValid @(SyncRequest Rational Rational)
    ordSpecOnValid @(SyncRequest Rational Rational)
    genValiditySpec @(SyncRequest Rational Rational)
    jsonSpecOnValid @(SyncRequest Rational Rational)
    eqSpecOnValid @(SyncResponse Rational Rational)
    ordSpecOnValid @(SyncResponse Rational Rational)
    genValiditySpec @(SyncResponse Rational Rational)
    jsonSpecOnValid @(SyncResponse Rational Rational)
    eqSpecOnValid @(CentralItem Rational)
    ordSpecOnValid @(CentralItem Rational)
    genValiditySpec @(CentralItem Rational)
    jsonSpecOnValid @(CentralItem Rational)
    eqSpecOnValid @(CentralStore Rational Rational)
    ordSpecOnValid @(CentralStore Rational Rational)
    genValiditySpec @(CentralStore Rational Rational)
    jsonSpecOnValid @(CentralStore Int Rational)
    describe "emptyStore" $
        it "is valid" $ shouldBeValid (emptyStore @Rational @Rational)
    describe "storeSize" $
        it "does not crash" $
        producesValidsOnValids (storeSize @Rational @Rational)
    describe "addItemToStore" $
        it "produces valid stores" $
        producesValidsOnValids2 (addItemToStore @Rational @Rational)
    describe "deleteUnsynced" $
        it "produces valid stores" $
        producesValidsOnValids2 (deleteUnsynced @Rational @Rational)
    describe "deleteSynced" $
        it "produces valid stores" $
        producesValidsOnValids2 (deleteSynced @Rational @Rational)
    describe "storeSize" $ do
        specify
            "deleting an unsynced item after adding it leaves the store with the original size" $
            forAllValid $ \store ->
                forAllValid $ \added ->
                    let size1 = storeSize (store :: Store Rational Rational)
                        store' = addItemToStore added store
                        store'' = deleteUnsynced added store'
                        size2 = storeSize store''
                     in size2 `shouldBe` size1
        specify
            "deleting a synced item after adding it leaves the store with the original size" $
            forAllValid $ \store ->
                forAllValid $ \synced ->
                    let size1 = storeSize (store :: Store Rational Rational)
                        store' =
                            Store $
                            S.insert (SyncedItem synced) $ storeItems store
                        store'' = deleteSynced synced store'
                        size2 = storeSize store''
                     in size2 `shouldBe` size1
    describe "makeSyncRequest" $
        it "produces valid sync requests" $
        producesValidsOnValids (makeSyncRequest @Rational @Rational)
    describe "mergeSyncResponse" $
        it "produces valid sync stores" $
        producesValidsOnValids2 (mergeSyncResponse @Rational @Rational)
    describe "processSyncWith" $ do
        it
            "makes no change if the sync request reflects the same local state with an empty sync response" $
            forAllValid $ \synct ->
                forAllValid $ \sis -> do
                    let cs = CentralStore sis
                    let (sr, cs') =
                            evalD $
                            processSyncWith
                                @(UUID Rational)
                                @Rational
                                genD
                                synct
                                cs $
                            SyncRequest S.empty (M.keysSet sis) S.empty
                    cs' `shouldBe` cs
                    sr `shouldBe` SyncResponse S.empty S.empty S.empty
        it "deletes the deleted items" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sreq -> do
                        let (_, cs') =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    sreq
                        syncRequestUndeletedItems sreq `shouldSatisfy`
                            (not .
                             any
                                 (`S.member` (M.keysSet $ centralStoreItems cs')))
        it "returns the items that were added in the sync response" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sreq -> do
                        let (sresp, _) =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    sreq
                        S.map syncedValue (syncResponseAddedItems sresp) `shouldBe`
                            S.map addedValue (syncRequestAddedItems sreq)
        it "returns the single added item" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \ai -> do
                        let (sresp, _) =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    SyncRequest
                                        { syncRequestAddedItems = S.singleton ai
                                        , syncRequestSyncedItems = S.empty
                                        , syncRequestUndeletedItems = S.empty
                                        }
                        S.map syncedValue (syncResponseAddedItems sresp) `shouldBe`
                            S.singleton (addedValue ai)
        it "adds the items that were added" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sreq -> do
                        let (_, cs') =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    sreq
                        S.map addedValue (syncRequestAddedItems sreq) `shouldSatisfy`
                            all
                                (`elem` (M.elems $
                                         M.map centralValue $
                                         centralStoreItems cs'))
        it
            "returns the single remotely added item if the sync request is empty and the central store has one item" $
            forAllValid $ \synct ->
                forAllValid $ \(uuid, ci) -> do
                    let (sresp, _) =
                            evalD $
                            processSyncWith
                                @(UUID Rational)
                                @Rational
                                genD
                                synct
                                (CentralStore $ M.singleton uuid ci)
                                SyncRequest
                                    { syncRequestAddedItems = S.empty
                                    , syncRequestSyncedItems = S.empty
                                    , syncRequestUndeletedItems = S.empty
                                    }
                    S.map syncedValue (syncResponseNewRemoteItems sresp) `shouldBe`
                        S.singleton (centralValue ci)
        it
            "returns all remotely added items when no items are locally added or deleted" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sis -> do
                        let (sresp, _) =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    SyncRequest
                                        { syncRequestAddedItems = S.empty
                                        , syncRequestSyncedItems = sis
                                        , syncRequestUndeletedItems = S.empty
                                        }
                        S.map syncedUuid (syncResponseNewRemoteItems sresp) `shouldBe`
                            S.difference (M.keysSet $ centralStoreItems cs) sis
        it "returns all remotely added items when no items are locally deleted " $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sis ->
                        forAllValid $ \ais -> do
                            let (sresp, _) =
                                    evalD $
                                    processSyncWith
                                        @(UUID Rational)
                                        @Rational
                                        genD
                                        synct
                                        cs
                                        SyncRequest
                                            { syncRequestAddedItems = ais
                                            , syncRequestSyncedItems = sis
                                            , syncRequestUndeletedItems =
                                                  S.empty
                                            }
                            S.map syncedUuid (syncResponseNewRemoteItems sresp) `shouldBe`
                                S.difference
                                    (M.keysSet $ centralStoreItems cs)
                                    sis
        it
            "returns all remotely added items that weren't deleted when no items are locally added " $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sis ->
                        forAllValid $ \dis -> do
                            let (sresp, _) =
                                    evalD $
                                    processSyncWith
                                        @(UUID Rational)
                                        @Rational
                                        genD
                                        synct
                                        cs
                                        SyncRequest
                                            { syncRequestAddedItems = S.empty
                                            , syncRequestSyncedItems = sis
                                            , syncRequestUndeletedItems = dis
                                            }
                            S.map syncedUuid (syncResponseNewRemoteItems sresp) `shouldBe`
                                S.difference
                                    (S.difference
                                         (M.keysSet $ centralStoreItems cs)
                                         dis)
                                    sis
        it "returns all remotely added items that weren't deleted" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sreq -> do
                        let (sresp, _) =
                                evalD $
                                processSyncWith
                                    @(UUID Rational)
                                    @Rational
                                    genD
                                    synct
                                    cs
                                    sreq
                        S.map syncedUuid (syncResponseNewRemoteItems sresp) `shouldBe`
                            S.difference
                                (S.difference
                                     (M.keysSet $ centralStoreItems cs)
                                     (syncRequestUndeletedItems sreq))
                                (syncRequestSyncedItems sreq)
        it
            "successfully syncs two clients using a central store when using incrementing words" $
            forAllValid $ \store1 ->
                forAllValid $ \(synct1, synct2, synct3) -> do
                    let (s1, s2) =
                            evalI $ do
                                let central = CentralStore M.empty
                                let store2 = Store S.empty
                                let sreq1 =
                                        makeSyncRequest @Word @Rational store1
                                (sresp1, central') <-
                                    processSyncWith genI synct1 central sreq1
                                let store1' = mergeSyncResponse store1 sresp1
                                let sreq2 = makeSyncRequest store2
                                (sresp2, central'') <-
                                    processSyncWith genI synct2 central' sreq2
                                let store2' = mergeSyncResponse store2 sresp2
                                let sreq3 = makeSyncRequest store1'
                                (sresp3, _) <-
                                    processSyncWith genI synct3 central'' sreq3
                                let store1'' = mergeSyncResponse store1' sresp3
                                pure (store1'', store2')
                    s1 `shouldBe` s2
        it
            "successfully syncs two clients using a central store when using deterministic UUIDs" $
            forAllValid $ \store1 ->
                forAllValid $ \(synct1, synct2, synct3) -> do
                    let (s1, s2) =
                            evalD $ do
                                let central = CentralStore M.empty
                                let store2 = Store S.empty
                                let sreq1 =
                                        makeSyncRequest
                                            @(UUID Rational)
                                            @Rational
                                            store1
                                (sresp1, central') <-
                                    processSyncWith genD synct1 central sreq1
                                let store1' = mergeSyncResponse store1 sresp1
                                let sreq2 = makeSyncRequest store2
                                (sresp2, central'') <-
                                    processSyncWith genD synct2 central' sreq2
                                let store2' = mergeSyncResponse store2 sresp2
                                let sreq3 = makeSyncRequest store1'
                                (sresp3, _) <-
                                    processSyncWith genD synct3 central'' sreq3
                                let store1'' = mergeSyncResponse store1' sresp3
                                pure (store1'', store2')
                    s1 `shouldBe` s2
        it
            "produces valid results when building up a central store from nothing using incrementing words" $
            forAllValid $ \tups ->
                shouldBeValid $
                evalI $ do
                    let initCentralStore = CentralStore M.empty
                    let go cs (store, synct) = do
                            let sreq = makeSyncRequest @Word @Rational store
                            (_, central') <- processSyncWith genI synct cs sreq
                            pure central'
                    foldM
                        go
                        initCentralStore
                        (tups :: [(Store Word Rational, UTCTime)])
        it
            "produces valid results when building up a central store from nothing using deterministic UUIDs" $
            forAllValid $ \tups ->
                shouldBeValid $
                evalD $ do
                    let initCentralStore = CentralStore M.empty
                    let go cs (store, synct) = do
                            let sreq =
                                    makeSyncRequest
                                        @(UUID Rational)
                                        @Rational
                                        store
                            (_, central') <- processSyncWith genD synct cs sreq
                            pure central'
                    foldM
                        go
                        initCentralStore
                        (tups :: [(Store (UUID Rational) Rational, UTCTime)])
        -- This property does not hold.
        xit "produces valid results when using incrementing words" $
            producesValidsOnValids3 $ \synct cs sr ->
                evalI $ processSyncWith @Word @Rational genI synct cs sr
        it "produces valid results when using determinisitic UUIDs" $
            producesValidsOnValids3 $ \synct cs sr ->
                evalD $
                processSyncWith @(UUID Rational) @Rational genD synct cs sr
        it "makes syncing idempotent with incrementing words" $
            forAllValid $ \synct1 ->
                forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
                    forAllValid $ \central1 ->
                        forAllValid $ \local1 -> do
                            let d1 = 0
                            let sreq1 = makeSyncRequest @Word @Rational local1
                            let ((sresp1, central2), d2) =
                                    runI
                                        (processSyncWith
                                             genI
                                             synct1
                                             central1
                                             sreq1)
                                        d1
                            let local2 = mergeSyncResponse local1 sresp1
                            let sreq2 = makeSyncRequest local2
                            let ((sresp2, central3), _) =
                                    runI
                                        (processSyncWith
                                             genI
                                             synct2
                                             central2
                                             sreq2)
                                        d2
                            let local3 = mergeSyncResponse local2 sresp2
                            local2 `shouldBe` local3
                            central2 `shouldBe` central3
        it "makes syncing idempotent with deterministic UUIDs" $
            forAllValid $ \synct1 ->
                forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
                    forAllValid $ \central1 ->
                        forAllValid $ \local1 -> do
                            let d1 = mkStdGen 42
                            let sreq1 =
                                    makeSyncRequest
                                        @(UUID Rational)
                                        @Rational
                                        local1
                            let ((sresp1, central2), d2) =
                                    runD
                                        (processSyncWith
                                             genD
                                             synct1
                                             central1
                                             sreq1)
                                        d1
                            let local2 = mergeSyncResponse local1 sresp1
                            let sreq2 = makeSyncRequest local2
                            let ((sresp2, central3), _) =
                                    runD
                                        (processSyncWith
                                             genD
                                             synct2
                                             central2
                                             sreq2)
                                        d2
                            let local3 = mergeSyncResponse local2 sresp2
                            local2 `shouldBe` local3
                            central2 `shouldBe` central3
        it "makes syncing idempotent with random UUIDs" $
            forAllValid $ \synct1 ->
                forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
                    forAllValid $ \central1 ->
                        forAllValid $ \local1 -> do
                            let sreq1 =
                                    makeSyncRequest
                                        @(UUID Rational)
                                        @Rational
                                        local1
                            (sresp1, central2) <-
                                processSyncWith
                                    nextRandomUUID
                                    synct1
                                    central1
                                    sreq1
                            let local2 = mergeSyncResponse local1 sresp1
                            let sreq2 = makeSyncRequest local2
                            (sresp2, central3) <-
                                processSyncWith
                                    nextRandomUUID
                                    synct2
                                    central2
                                    sreq2
                            let local3 = mergeSyncResponse local2 sresp2
                            local2 `shouldBe` local3
                            central2 `shouldBe` central3
    describe "processSync" $
        it "makes syncing idempotent when using random UUIDs" $
        forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
                let sreq1 = makeSyncRequest @(UUID Rational) @Rational local1
                (sresp1, central2) <- processSync nextRandomUUID central1 sreq1
                let local2 = mergeSyncResponse local1 sresp1
                let sreq2 = makeSyncRequest local2
                (sresp2, central3) <- processSync nextRandomUUID central2 sreq2
                let local3 = mergeSyncResponse local2 sresp2
                local2 `shouldBe` local3
                central2 `shouldBe` central3

newtype D a = D
    { unD :: State StdGen a
    } deriving (Generic, Functor, Applicative, Monad, MonadState StdGen)

evalD :: D a -> a
evalD d = fst $ runD d $ mkStdGen 42

runD :: D a -> StdGen -> (a, StdGen)
runD = runState . unD

genD :: D (Typed.UUID a)
genD = do
    r <- get
    let (u, r') = random r
    put r'
    pure u

newtype I a = I
    { unI :: State Word a
    } deriving (Generic, Functor, Applicative, Monad, MonadState Word)

evalI :: I a -> a
evalI i = fst $ runI i 0

runI :: I a -> Word -> (a, Word)
runI = runState . unI

genI :: I Word
genI = do
    i <- get
    modify succ
    pure i
