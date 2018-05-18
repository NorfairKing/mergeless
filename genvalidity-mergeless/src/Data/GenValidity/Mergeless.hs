{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeless where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Data.Mergeless

instance (GenUnchecked a, Ord a) => GenUnchecked (Store a)

instance (GenValid a, Ord a) => GenValid (Store a) where
    genValid = Store <$> genValid

instance (GenInvalid a, Ord a) => GenInvalid (Store a) where
    genInvalid = Store <$> genInvalid

instance GenUnchecked a => GenUnchecked (StoreItem a)

instance GenValid a => GenValid (StoreItem a)

instance GenInvalid a => GenInvalid (StoreItem a)

instance GenUnchecked a => GenUnchecked (Added a)

instance GenValid a => GenValid (Added a) where
    genValid = Added <$> genValid <*> genValid

instance GenInvalid a => GenInvalid (Added a)

instance GenUnchecked a => GenUnchecked (Synced a)

instance GenValid a => GenValid (Synced a) where
    genValid = Synced <$> genValid <*> genValid <*> genValid <*> genValid

instance GenInvalid a => GenInvalid (Synced a)
