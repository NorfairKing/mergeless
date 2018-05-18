{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()

import Data.Mergeless

instance (GenUnchecked a, Ord a) => GenUnchecked (Store a)

instance (GenValid a, Ord a) => GenValid (Store a) where
    genValid =( Store <$> genValid) `suchThat` isValid

instance (GenInvalid a, Ord a) => GenInvalid (Store a)

instance GenUnchecked a => GenUnchecked (StoreItem a)

instance GenValid a => GenValid (StoreItem a)

instance GenInvalid a => GenInvalid (StoreItem a)

instance GenUnchecked a => GenUnchecked (Added a)

instance GenValid a => GenValid (Added a) where
    genValid = (Added <$> genValid <*> genValid) `suchThat` isValid

instance GenInvalid a => GenInvalid (Added a)

instance GenUnchecked a => GenUnchecked (Synced a)

instance GenValid a => GenValid (Synced a) where
    genValid =
        (Synced <$> genValid <*> genValid <*> genValid <*> genValid) `suchThat`
        isValid

instance GenInvalid a => GenInvalid (Synced a)

instance (GenUnchecked a, Ord a) => GenUnchecked (SyncRequest a)

instance (GenValid a, Ord a) => GenValid (SyncRequest a) where
    genValid =
        (SyncRequest <$> genValid <*> genValid <*> genValid) `suchThat`
        isValid

instance (GenInvalid a, Ord a) => GenInvalid (SyncRequest a)

instance (GenUnchecked a, Ord a) => GenUnchecked (SyncResponse a)

instance (GenValid a, Ord a) => GenValid (SyncResponse a) where
    genValid =
        (SyncResponse <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance (GenInvalid a, Ord a) => GenInvalid (SyncResponse a)
