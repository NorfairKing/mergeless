{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless.Collection where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import Control.Monad

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
  genValid = do
    identifiers <- scale (* 4) genValid
    (s1, s2) <- splitSet identifiers
    (s3, s4) <- splitSet s1
    (s5, s6) <- splitSet s2
    syncResponseClientAdded <-
      fmap M.fromList $
      forM (S.toList s3) $ \i -> do
        cid <- genValid -- TODO maybe we can find a way to not generate duplicate client ids and speed up this generator, but it's fine for now.
        t <- genValid
        pure (cid, (i, t))
    let syncResponseClientDeleted = s4
    syncResponseServerAdded <- mapWithIds s5
    let syncResponseServerDeleted = s6
    pure SyncResponse {..}
  shrinkValid = shrinkValidStructurally

splitSet :: Ord i => Set i -> Gen (Set i, Set i)
splitSet s =
  if S.null s
    then pure (S.empty, S.empty)
    else do
      a <- elements $ S.toList s
      pure $ S.split a s

mapWithIds :: (Ord i, GenValid a) => Set i -> Gen (Map i a)
mapWithIds s = fmap M.fromList $ forM (S.toList s) $ \i -> (,) i <$> genValid

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
