{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless.Item where

import qualified Data.Set as S

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Test.QuickCheck

import Data.Mergeless.Item

instance GenUnchecked a => GenUnchecked (Added a)

instance GenValid a => GenValid (Added a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (Synced a)

instance GenValid a => GenValid (Synced a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
instance GenUnchecked a => GenUnchecked (ClientItem a)

instance GenValid a => GenValid (ClientItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
instance GenUnchecked a => GenUnchecked (SyncRequest a)

instance GenValid a => GenValid (SyncRequest a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
instance GenUnchecked a => GenUnchecked (SyncResponse a)

instance GenValid a => GenValid (SyncResponse a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
instance GenUnchecked a => GenUnchecked (ServerItem a)

instance GenValid a => GenValid (ServerItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
