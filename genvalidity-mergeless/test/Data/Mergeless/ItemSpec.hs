{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Mergeless.ItemSpec
  ( spec
  ) where

import Data.Int (Int)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless.Item ()
import Data.GenValidity.UUID.Typed ()
import Data.Mergeless.Item
import Data.UUID.Typed

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  eqSpecOnValid @(Added Int)
  genValidSpec @(Added Int)
  jsonSpecOnValid @(Added Int)
  eqSpecOnValid @(Synced Int)
  genValidSpec @(Synced Int)
  jsonSpecOnValid @(Synced Int)
  eqSpecOnValid @(ClientItem Int)
  genValidSpec @(ClientItem Int)
  jsonSpecOnValid @(ClientItem Int)
  eqSpecOnValid @(SyncRequest Int)
  genValidSpec @(SyncRequest Int)
  jsonSpecOnValid @(SyncRequest Int)
  eqSpecOnValid @(SyncResponse Int)
  genValidSpec @(SyncResponse Int)
  jsonSpecOnValid @(SyncResponse Int)
  eqSpecOnValid @(ServerItem Int)
  genValidSpec @(ServerItem Int)
  jsonSpecOnValid @(ServerItem Int)
  describe "makeItemSyncrequest" $
    it "produces valid requests" $ producesValidsOnValids (makeItemSyncRequest @Int)
  describe "mergeSyncResponse" $
    it "produces valid client items" $ producesValidsOnValids2 (mergeSyncResponse @Int)
  describe "processItemSync" $
    it "produces valid tuples" $ producesValidsOnValids3 (processItemSync @Int)

newtype D a =
  D
    { unD :: State StdGen a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen)

evalD :: D a -> a
evalD d = fst $ runD d $ mkStdGen 42

runD :: D a -> StdGen -> (a, StdGen)
runD = runState . unD

genD :: D (Typed.UUID a)
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u

newtype I a =
  I
    { unI :: State Word a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState Word)

evalI :: I a -> a
evalI i = fst $ runI i 0

runI :: I a -> Word -> (a, Word)
runI = runState . unI

genI :: I Word
genI = do
  i <- get
  modify succ
  pure i
