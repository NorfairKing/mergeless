{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless where

import qualified Data.Set as S

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Test.QuickCheck

import Data.Mergeless

instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (Store i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (Store i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (Store i a)

instance (GenUnchecked i, GenUnchecked a) => GenUnchecked (StoreItem i a)

instance (GenValid i, GenValid a) => GenValid (StoreItem i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a) =>
         GenInvalid (StoreItem i a)

instance GenUnchecked a => GenUnchecked (Added a)

instance GenValid a => GenValid (Added a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked a, GenInvalid a) => GenInvalid (Added a)

instance (GenUnchecked i, GenUnchecked a) => GenUnchecked (Synced i a)

instance (GenValid i, GenValid a) => GenValid (Synced i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a) =>
         GenInvalid (Synced i a)

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

instance GenUnchecked a => GenUnchecked (CentralItem a)

instance GenValid a => GenValid (CentralItem a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked a, GenInvalid a) => GenInvalid (CentralItem a)

instance (GenUnchecked i, GenUnchecked a, Ord i, Ord a) => GenUnchecked (CentralStore i a)

instance (GenValid i, GenValid a, Ord i, Ord a) => GenValid (CentralStore i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a, GenInvalid i, GenInvalid a, Ord i, Ord a) =>
         GenInvalid (CentralStore i a)

genUnsyncedStore ::
     forall i a. (Ord i, Ord a, GenValid i, GenValid a, Num i)
  => Gen (Store i a)
genUnsyncedStore = do
  storeItems <- S.fromList <$> (genListOf $ UnsyncedItem <$> genValid)
  pure Store {..}
