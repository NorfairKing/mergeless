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
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless.Item ()
import Data.GenValidity.UUID ()
import Data.Mergeless.Item
import Data.UUID
import Data.UUID.V4

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
  eqSpecOnValid @(ItemSyncRequest Int)
  genValidSpec @(ItemSyncRequest Int)
  jsonSpecOnValid @(ItemSyncRequest Int)
  eqSpecOnValid @(ItemSyncResponse Int)
  genValidSpec @(ItemSyncResponse Int)
  jsonSpecOnValid @(ItemSyncResponse Int)
  eqSpecOnValid @(ServerItem Int)
  genValidSpec @(ServerItem Int)
  jsonSpecOnValid @(ServerItem Int)
  describe "makeItemSyncrequest" $
    it "produces valid requests" $ producesValidsOnValids (makeItemSyncRequest @Int)
  describe "mergeItemSyncResponse" $
    it "produces valid client items" $ producesValidsOnValids2 (mergeItemSyncResponse @Int)
  describe "processItemSync" $
    it "produces valid tuples" $ producesValidsOnValids3 (processServerItemSync @Int)
  describe "processItemSync" $ do
    describe "Interaction" $ do
      it "makes no changes if the sync request reflects the state of the empty server" $
        forAllValid $ \t -> do
          let store1 = ServerItemEmpty
              req = ItemSyncRequestPoll
          let (resp, store2) = processServerItemSync @Int t store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ItemSyncResponseInSyncEmpty
      it "makes no changes if the sync request reflects the state of the full server" $
        forAllValid $ \t ->
          forAllValid $ \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestKnown
            let (resp, store2) = processServerItemSync @Int t store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseInSyncFull
    describe "Client changes" $ do
      it "adds the item that the client tells the server to add" $
        forAllValid $ \t ->
          forAllValid $ \a -> do
            let store1 = ServerItemEmpty
                req = ItemSyncRequestNew a
            let (resp, store2) = processServerItemSync @Int t store1 req
            store2 `shouldBe` ServerItemFull (addedToSynced t a)
            resp `shouldBe` ItemSyncResponseClientAdded t
      it "deletes the item that the client tells the server to delete" $
        forAllValid $ \t ->
          forAllValid $ \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestDeleted
            let (resp, store2) = processServerItemSync @Int t store1 req
            store2 `shouldBe` ServerItemEmpty
            resp `shouldBe` ItemSyncResponseClientDeleted
    describe "Server changes" $ do
      it "tells the client that there is a new item at the server side" $
        forAllValid $ \t ->
          forAllValid $ \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestPoll
            let (resp, store2) = processServerItemSync @Int t store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseServerAdded s
      it "tells the client that there is a deleted item at the server side" $
        forAllValid $ \t -> do
          let store1 = ServerItemEmpty
              req = ItemSyncRequestKnown
          let (resp, store2) = processServerItemSync @Int t store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ItemSyncResponseServerDeleted
    describe "High level properties" $ do
      it "it always possible to add an item from scratch" $
        forAllValid $ \t ->
          forAllValid $ \a -> do
            let cstore1 = ClientAdded (a :: Added Int)
            let sstore1 = ServerItemEmpty
            let req1 = makeItemSyncRequest cstore1
                (resp1, sstore2) = processServerItemSync t sstore1 req1
                cstore2 = mergeItemSyncResponse cstore1 resp1
            resp1 `shouldBe` ItemSyncResponseClientAdded t
            let s = addedToSynced t a
            sstore2 `shouldBe` ServerItemFull s
            cstore2 `shouldBe` ClientSynced s
      it "is idempotent with one client" $
        forAllValid $ \t1 ->
          forAllValid $ \t2 ->
            forAllValid $ \si1 ->
              forAllValid $ \ci1 -> do
                let req1 = makeItemSyncRequest @Int ci1
                    (resp1, si2) = processServerItemSync t1 si1 req1
                    ci2 = mergeItemSyncResponse ci1 resp1
                    req2 = makeItemSyncRequest ci2
                    (resp2, si3) = processServerItemSync t2 si2 req2
                    ci3 = mergeItemSyncResponse ci2 resp2
                ci3 `shouldBe` ci2
                si3 `shouldBe` si2

newtype D a =
  D
    { unD :: State StdGen a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen)

evalD :: D a -> a
evalD d = fst $ runD d $ mkStdGen 42

runD :: D a -> StdGen -> (a, StdGen)
runD = runState . unD

genD :: D UUID
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
