{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergelessSpec
    ( spec
    ) where

import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import Data.Word
import GHC.Generics (Generic)

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
    eqSpec @(Store Double)
    ordSpec @(Store Double)
    genValiditySpec @(Store Double)
    jsonSpecOnValid @(Store Double)
    eqSpec @(StoreItem Double)
    ordSpec @(StoreItem Double)
    genValiditySpec @(StoreItem Double)
    jsonSpecOnValid @(StoreItem Double)
    eqSpec @(Added Double)
    ordSpec @(Added Double)
    genValiditySpec @(Added Double)
    jsonSpecOnValid @(Added Double)
    eqSpec @(Synced Double)
    ordSpec @(Synced Double)
    genValiditySpec @(Synced Double)
    jsonSpecOnValid @(Synced Double)
    eqSpec @(SyncRequest Double)
    ordSpec @(SyncRequest Double)
    genValiditySpec @(SyncRequest Double)
    jsonSpecOnValid @(SyncRequest Double)
    eqSpec @(SyncResponse Double)
    ordSpec @(SyncResponse Double)
    genValiditySpec @(SyncResponse Double)
    jsonSpecOnValid @(SyncResponse Double)
    eqSpec @(CentralStore Double)
    ordSpec @(CentralStore Double)
    genValiditySpec @(CentralStore Double)
    jsonSpecOnValid @(CentralStore Double)
    describe "makeSyncRequest" $
        it "produces valid sync requests" $
        producesValidsOnValids (makeSyncRequest @Double)
    describe "mergeSyncResponse" $
        it "produces valid sync stores" $
        producesValidsOnValids2 (mergeSyncResponse @Double)
    describe "processSyncWith" $ do
        it "makes no change if the sync request is empty" $
            forAllValid $ \synct ->
                forAllValid $ \cs -> do
                    let (_, cs') =
                            evalD $
                            processSyncWith
                                @Double
                                genD
                                synct
                                cs
                                (SyncRequest S.empty S.empty S.empty)
                    cs' `shouldBe` cs
        it "produces valid results when using determinisitic UUIDs" $
            producesValidsOnValids3 $ \synct cs sr ->
                evalD $ processSyncWith @Double genD synct cs sr
        it "makes syncing idempotent with deterministic UUIDs" $
            forAllValid $ \synct1 ->
                forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
                    forAllValid $ \central1 ->
                        forAllValid $ \local1 -> do
                            let d1 = 0
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
    { unD :: State Word32 a
    } deriving (Generic, Functor, Applicative, Monad, MonadState Word32)

evalD :: D a -> a
evalD d = fst $ runD d 0

runD :: D a -> Word32 -> (a, Word32)
runD = runState . unD

genD :: D (Typed.UUID a)
genD = do
    w <- get
    modify (+ 1)
    pure $ Typed.UUID $ UUID.fromWords 0 0 0 w -- TODO use the entire space? probably not necessary.
