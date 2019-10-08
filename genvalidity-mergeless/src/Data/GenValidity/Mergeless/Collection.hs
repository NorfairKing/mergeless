{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless.Collection where

import qualified Data.Set as S

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Test.QuickCheck

import Data.GenValidity.Mergeless.Item
import Data.Mergeless

instance GenUnchecked ClientId
instance GenValid ClientId
instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (ClientStore i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (ClientStore i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (ClientStore i a)

instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (SyncRequest i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (SyncRequest i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (SyncRequest i a)

instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (SyncResponse i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (SyncResponse i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (SyncResponse i a)

instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (ServerStore i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (ServerStore i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (ServerStore i a)

genUnsyncedStore ::
     forall i a. (Ord i, Ord a, GenValid i, GenValid a)
  => Gen (ClientStore i a)
genUnsyncedStore = do
  as <- genValid
  pure $ emptyClientStore {clientStoreAdded = as}
