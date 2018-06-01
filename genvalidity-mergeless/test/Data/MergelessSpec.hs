{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergelessSpec
    ( spec
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless ()
import Data.Mergeless
import Data.UUID.Typed

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
    eqSpecOnValid @(Store Double)
    ordSpecOnValid @(Store Double)
    genValiditySpec @(Store Double)
    jsonSpecOnValid @(Store Double)
    eqSpecOnValid @(StoreItem Double)
    ordSpecOnValid @(StoreItem Double)
    genValiditySpec @(StoreItem Double)
    jsonSpecOnValid @(StoreItem Double)
    eqSpecOnValid @(Added Double)
    ordSpecOnValid @(Added Double)
    genValiditySpec @(Added Double)
    jsonSpecOnValid @(Added Double)
    eqSpecOnValid @(Synced Double)
    ordSpecOnValid @(Synced Double)
    genValiditySpec @(Synced Double)
    jsonSpecOnValid @(Synced Double)
    eqSpecOnValid @(SyncRequest Double)
    ordSpecOnValid @(SyncRequest Double)
    genValiditySpec @(SyncRequest Double)
    jsonSpecOnValid @(SyncRequest Double)
    eqSpecOnValid @(SyncResponse Double)
    ordSpecOnValid @(SyncResponse Double)
    genValiditySpec @(SyncResponse Double)
    jsonSpecOnValid @(SyncResponse Double)
    eqSpecOnValid @(CentralItem Double)
    ordSpecOnValid @(CentralItem Double)
    genValiditySpec @(CentralItem Double)
    jsonSpecOnValid @(CentralItem Double)
    eqSpecOnValid @(CentralStore Double)
    ordSpecOnValid @(CentralStore Double)
    genValiditySpec @(CentralStore Double)
    jsonSpecOnValid @(CentralStore Double)
    describe "makeSyncRequest" $
        it "produces valid sync requests" $
        producesValidsOnValids (makeSyncRequest @Double)
    describe "mergeSyncResponse" $
        it "produces valid sync stores" $
        producesValidsOnValids2 (mergeSyncResponse @Double)
    describe "processSyncWith" $ do
        it
            "makes no change if the sync request reflects the same local state with an empty sync response" $
            forAllValid $ \synct ->
                forAllValid $ \sis -> do
                    let cs = CentralStore sis
                    let (sr, cs') =
                            evalD $
                            processSyncWith @Double genD synct cs $
                            SyncRequest S.empty (M.keysSet sis) S.empty
                    cs' `shouldBe` cs
                    sr `shouldBe` SyncResponse S.empty S.empty S.empty
        it "deletes the deleted items" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \sreq -> do
                        let (_, cs') =
                                evalD $
                                processSyncWith @Double genD synct cs sreq
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
                                processSyncWith @Double genD synct cs sreq
                        S.map syncedValue (syncResponseAddedItems sresp) `shouldBe`
                            S.map addedValue (syncRequestAddedItems sreq)
        it "returns the single added item" $
            forAllValid $ \synct ->
                forAllValid $ \cs ->
                    forAllValid $ \ai -> do
                        let (sresp, _) =
                                evalD $
                                processSyncWith
                                    @Double
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
                                processSyncWith @Double genD synct cs sreq
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
                                @Double
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
                                    @Double
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
                                        @Double
                                        genD
                                        synct
                                        cs
                                        SyncRequest
                                        { syncRequestAddedItems = ais
                                        , syncRequestSyncedItems = sis
                                        , syncRequestUndeletedItems = S.empty
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
                                        @Double
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
                                processSyncWith @Double genD synct cs sreq
                        S.map syncedUuid (syncResponseNewRemoteItems sresp) `shouldBe`
                            S.difference
                                (S.difference
                                     (M.keysSet $ centralStoreItems cs)
                                     (syncRequestUndeletedItems sreq))
                                (syncRequestSyncedItems sreq)
        it "produces valid results when using determinisitic UUIDs" $
            producesValidsOnValids3 $ \synct cs sr ->
                evalD $ processSyncWith @Double genD synct cs sr
        it "makes syncing idempotent with deterministic UUIDs" $
            forAllValid $ \synct1 ->
                forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
                    forAllValid $ \central1 ->
                        forAllValid $ \local1 -> do
                            let d1 = mkStdGen 42
                            let sreq1 = makeSyncRequest @Double local1
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
                            let sreq1 = makeSyncRequest @Double local1
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
        it "makes syncing idempotent" $
        forAllValid $ \central1 ->
            forAllValid $ \local1 -> do
                let sreq1 = makeSyncRequest @Double local1
                (sresp1, central2) <- processSync central1 sreq1
                let local2 = mergeSyncResponse local1 sresp1
                let sreq2 = makeSyncRequest local2
                (sresp2, central3) <- processSync central2 sreq2
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
