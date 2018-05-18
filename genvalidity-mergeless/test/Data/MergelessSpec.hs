{-# LANGUAGE TypeApplications #-}

module Data.MergelessSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless ()
import Data.Mergeless

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
    describe "makeSyncRequest" $
        it "produces valid sync requests" $
        producesValidsOnValids (makeSyncRequest @Double)
    describe "mergeSyncResponse" $
        it "produces valid sync stores" $
        producesValidsOnValids2 (mergeSyncResponse @Double)
