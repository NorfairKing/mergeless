{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless where

import Control.Monad
import qualified Data.Map as M
import Data.Set (Set)
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

genUnsyncedStore :: forall i a. (Ord i, Ord a, GenValid i, GenValid a, Num i) => Gen (Store i a)
genUnsyncedStore = do
  sized $ \n -> do
    part <- arbPartition n
    (_, storeItems) <-
      foldM
        (\(ks, items) s -> do
           (item, mi) <-
             resize s $
             oneof
               [ do i <- genIdGreaterThan ks
                    let item = UndeletedItem i
                    pure (item, Just i)
               , do item <- UnsyncedItem <$> genValid
                    pure (item, Nothing)
               ]
           pure $
             case mi of
               Nothing -> (ks, S.insert item items)
               Just i -> (S.insert i ks, S.insert item items))
        (S.empty, S.empty)
        part
    pure Store {..}

genStoreFor ::
     forall i a. (Ord i, Ord a, GenValid i, GenValid a, Num i)
  => CentralStore i a
  -> Gen (Store i a)
genStoreFor cs = genStoreForKeys $ M.keysSet $ centralStoreItems cs

genStoreForKeys ::
     forall i a. (Ord i, Ord a, GenValid i, GenValid a, Num i)
  => Set i
  -> Gen (Store i a)
genStoreForKeys keys =
  sized $ \n -> do
    part <- arbPartition n
    (_, storeItems) <-
      foldM
        (\(ks, items) s -> do
           (item, mi) <-
             resize s $
             oneof
               [ do i <- genIdGreaterThan ks
                    let item = UndeletedItem i
                    pure (item, Just i)
               , do item <- UnsyncedItem <$> genValid
                    pure (item, Nothing)
               , do i <- genIdGreaterThan ks
                    s_ <- genValid :: Gen (Synced i a)
                    let item = SyncedItem $ s_ {syncedUuid = i}
                    pure (item, Just i)
               ]
           pure $
             case mi of
               Nothing -> (ks, S.insert item items)
               Just i -> (S.insert i ks, S.insert item items))
        (keys, S.empty)
        part
    pure Store {..}

genSyncRequestFor ::
     forall i a. (Ord i, Ord a, GenValid i, GenValid a, Num i)
  => CentralStore i a
  -> Gen (SyncRequest i a)
genSyncRequestFor cs = do
  syncRequestAddedItems <- genValid
  let keys = M.keysSet $ centralStoreItems cs
  syncRequestSyncedItems <- genSetGreaterThan keys
  syncRequestUndeletedItems <- genSetGreaterThan $ keys `S.union` syncRequestSyncedItems
  pure SyncRequest {..}

genSetGreaterThan :: (Ord i, Num i, GenValid i) => Set i -> Gen (Set i)
genSetGreaterThan keys =
  sized $ \n -> do
    part <- arbPartition n
    foldM
      (\ks s -> do
         i <- resize s $ genIdGreaterThan ks
         pure $ S.insert i ks)
      keys
      part

genIdGreaterThan :: (Ord i, Num i, GenValid i) => Set i -> Gen i
genIdGreaterThan keys =
  let m =
        case S.lookupMax keys of
          Nothing -> 0
          Just i -> i
   in genValid `suchThat` (> m)
