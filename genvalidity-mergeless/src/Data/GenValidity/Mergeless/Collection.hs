{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless.Collection where

import Control.Monad
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Mergeless.Item ()
import Data.GenValidity.Time ()
import qualified Data.Map as M
import Data.Map (Map)
import Data.Mergeless
import qualified Data.Set as S
import Data.Set (Set)
import Test.QuickCheck

instance GenUnchecked ClientId

instance GenValid ClientId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) =>
  GenUnchecked (ClientStore ci si a)

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (ClientStore ci si a) where
  genValid =
    sized $ \n -> do
      (a, b, c) <- genSplit3 n
      (s1, s2) <- resize (a + b) (genValid >>= splitSet)
      clientStoreAdded <- resize c genValid
      clientStoreSynced <- mapWithIds s1
      let clientStoreDeleted = s2
      pure ClientStore {..}
  shrinkValid = shrinkValidStructurally

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, GenInvalid ci, GenInvalid si, GenInvalid a, Ord ci, Ord si, Ord a) =>
  GenInvalid (ClientStore ci si a)

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) =>
  GenUnchecked (SyncRequest ci si a)

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (SyncRequest ci si a) where
  genValid =
    sized $ \n -> do
      (a, b, c) <- genSplit3 n
      (s1, s2) <- resize (a + b) (genValid >>= splitSet)
      syncRequestAdded <- resize c genValid
      let syncRequestSynced = s1
      let syncRequestDeleted = s2
      pure SyncRequest {..}
  shrinkValid = shrinkValidStructurally

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, GenInvalid ci, GenInvalid si, GenInvalid a, Ord ci, Ord si, Ord a) =>
  GenInvalid (SyncRequest ci si a)

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) =>
  GenUnchecked (SyncResponse ci si a)

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (SyncResponse ci si a) where
  genValid = do
    (s1, s2) <- genValid >>= splitSet
    (s3, s4) <- splitSet s1
    (s5, s6) <- splitSet s2
    syncResponseClientAdded <-
      fmap M.fromList
        $ forM (S.toList s3)
        $ \i -> do
          cid <- genValid -- TODO maybe we can find a way to not generate duplicate client ids and speed up this generator, but it's fine for now.
          pure (cid, i)
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
mapWithIds = traverse id . M.fromSet (const genValid)

instance
  (GenUnchecked ci, GenUnchecked si, GenUnchecked a, Show ci, Show si, Show a, GenInvalid ci, GenInvalid si, GenInvalid a, Ord ci, Ord si, Ord a) =>
  GenInvalid (SyncResponse ci si a)

instance (GenUnchecked si, GenUnchecked a, Ord si, Ord a) => GenUnchecked (ServerStore si a)

instance (GenValid si, GenValid a, Show si, Show a, Ord si, Ord a) => GenValid (ServerStore si a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance
  (GenUnchecked si, GenUnchecked a, Show si, Show a, GenInvalid si, GenInvalid a, Ord si, Ord a) =>
  GenInvalid (ServerStore si a)

genServerStoreFromSet :: (Ord si, GenValid v) => Set si -> Gen (ServerStore si v)
genServerStoreFromSet s = ServerStore <$> mapWithIds s

genUnsyncedStore ::
  forall ci si a.
  (Show ci, Ord ci, Ord si, Ord a, GenValid ci, GenValid si, GenValid a) =>
  Gen (ClientStore ci si a)
genUnsyncedStore = do
  as <- genValid
  pure $ emptyClientStore {clientStoreAdded = as}

genClientStoreFromSet :: (Show ci, Ord ci, Ord si, GenValid ci, GenValid v) => Set si -> Gen (ClientStore ci si v)
genClientStoreFromSet s = do
  (s1, s2) <- splitSet s
  clientStoreAdded <- genValid
  clientStoreSynced <- mapWithIds s1
  let clientStoreDeleted = s2
  pure ClientStore {..}
