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
