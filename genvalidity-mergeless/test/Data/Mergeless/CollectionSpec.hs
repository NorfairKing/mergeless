{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeless.CollectionSpec
  ( spec
  ) where

import Data.Int (Int)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeless.Collection
import Data.GenValidity.UUID ()
import Data.Mergeless.Collection
import Data.UUID

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  eqSpecOnValid @(ClientStore Int Int)
  ordSpecOnValid @(ClientStore Int Int)
  genValidSpec @(ClientStore Int Int)
  jsonSpecOnValid @(ClientStore Int Int)
  eqSpecOnValid @(SyncRequest Int Int)
  ordSpecOnValid @(SyncRequest Int Int)
  genValidSpec @(SyncRequest Int Int)
  jsonSpecOnValid @(SyncRequest Int Int)
  eqSpecOnValid @(SyncResponse Int Int)
  ordSpecOnValid @(SyncResponse Int Int)
  genValidSpec @(SyncResponse Int Int)
  jsonSpecOnValid @(SyncResponse Int Int)
  eqSpecOnValid @(ServerStore Int Int)
  ordSpecOnValid @(ServerStore Int Int)
  genValidSpec @(ServerStore Int Int)
  jsonSpecOnValid @(ServerStore Int Int)
  describe "emptyStore" $ it "is valid" $ shouldBeValid (emptyClientStore @Int @Int)
  describe "storeSize" $ do
    it "does not crash" $ producesValidsOnValids (storeSize @Int @Int)
    specify "deleting an unsynced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAllValid $ \added ->
          let size1 = storeSize (store :: ClientStore Int Int)
              store' = addItemToClientStore added store
           in case sortOn (Down . fst) $ M.toList (clientStoreAdded store') of
                [] -> expectationFailure "Expected a nonempty list"
                (i, _):_ ->
                  let store'' = deleteUnsyncedFromClientStore i store'
                      size2 = storeSize store''
                   in size2 `shouldBe` size1
    specify "deleting a synced item after adding it leaves the store with the original size" $
      forAllValid $ \store ->
        forAll (genValid `suchThat` (\uuid -> not $ M.member uuid $ clientStoreSynced store)) $ \uuid ->
          forAllValid $ \synced ->
            let size1 = storeSize (store :: ClientStore Int Int)
                store' = store {clientStoreSynced = M.insert uuid synced $ clientStoreSynced store}
                store'' = deleteSyncedFromClientStore uuid store'
                size2 = storeSize store''
             in size2 `shouldBe` size1
  describe "addItemToClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (addItemToClientStore @Int @Int)
  describe "deleteUnsyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteUnsyncedFromClientStore @Int @Int)
  describe "deleteSyncedFromClientStore" $
    it "produces valid stores" $ producesValidsOnValids2 (deleteSyncedFromClientStore @Int @Int)
  describe "makeSyncRequest" $
    it "produces valid sync requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeSyncResponse" $ do
    it "produces valid sync stores" $ producesValidsOnValids2 (mergeSyncResponse @Int @Int)
    it "deletes items that the server instructed to be deleted" $
      forAllValid $ \cs ->
        forAllValid $ \sr ->
          let cs' = mergeSyncResponse @Int @Int cs sr
           in clientStoreDeleted cs' `shouldBe`
              (clientStoreDeleted cs `S.difference` syncResponseClientDeleted sr)
  describe "processServerSyncWith" $ do
    describe "incrementing words" $ serverSyncSpec @Int evalI $ processServerSyncWith genI
    describe "deterministic UUIDs" $ serverSyncSpec @Int evalD $ processServerSyncWith genD

serverSyncSpec ::
     forall a i m. (Show i, Ord i, GenValid i, Show a, Ord a, GenValid a, Monad m)
  => (forall r. m r -> r)
  -> (UTCTime -> ServerStore i a -> SyncRequest i a -> m (SyncResponse i a, ServerStore i a))
  -> Spec
serverSyncSpec eval func = do
  it "makes no change if the sync request reflects the same local state with an empty sync response" $
    forAllValid $ \synct ->
      forAllValid $ \sis -> do
        let cs = ServerStore sis
        let (sr, cs') =
              eval $
              func synct cs $
              SyncRequest
                { syncRequestAdded = M.empty
                , syncRequestSynced = M.keysSet sis
                , syncRequestDeleted = S.empty
                }
        cs' `shouldBe` cs
        sr `shouldBe`
          SyncResponse
            { syncResponseClientAdded = M.empty
            , syncResponseClientDeleted = S.empty
            , syncResponseServerAdded = M.empty
            , syncResponseServerDeleted = S.empty
            }
  it "deletes the deleted items" $
    forAllValid $ \synct ->
      forAllValid $ \cs ->
        forAllValid $ \sreq -> do
          let (_, cs') = eval $ func synct cs sreq
          syncRequestDeleted sreq `shouldSatisfy`
            (not . any (`S.member` (M.keysSet $ serverStoreItems cs')))
  it "returns the items that were added in the sync response" $
    forAllValid $ \synct ->
      forAllValid $ \cs ->
        forAllValid $ \sreq -> do
          let (sresp, _) = eval $ func synct cs sreq
          M.keysSet (syncResponseClientAdded sresp) `shouldBe` M.keysSet (syncRequestAdded sreq)
  it "returns the single added item" $
    forAllValid $ \synct ->
      forAllValid $ \cs ->
        forAllValid $ \ai -> do
          let (sresp, _) =
                eval $
                func
                  synct
                  cs
                  SyncRequest
                    { syncRequestAdded = M.singleton (ClientId 0) ai
                    , syncRequestSynced = S.empty
                    , syncRequestDeleted = S.empty
                    }
          M.keysSet (syncResponseClientAdded sresp) `shouldBe` S.singleton (ClientId 0)
  it "adds the items that were added" $
    forAllValid $ \synct ->
      forAllValid $ \cs ->
        forAllValid $ \sreq -> do
          let (_, cs') = eval $ func synct cs sreq
          S.fromList (M.elems (M.map addedValue (syncRequestAdded sreq))) `shouldSatisfy`
            (`S.isSubsetOf` (S.fromList $ M.elems $ M.map syncedValue $ serverStoreItems cs'))
  it
    "returns the single remotely added item if the sync request is empty and the central store has one item" $
    forAllValid $ \synct ->
      forAllValid $ \(uuid, si) -> do
        let (sresp, _) =
              eval $
              func
                synct
                (ServerStore $ M.singleton uuid si)
                SyncRequest
                  { syncRequestAdded = M.empty
                  , syncRequestSynced = S.empty
                  , syncRequestDeleted = S.empty
                  }
        syncResponseServerAdded sresp `shouldBe` M.singleton uuid si
  it "returns all remotely added items when no items are locally added or deleted" $
    forAllValid $ \synct ->
      forAllValid $ \ss ->
        forAllValid $ \sis -> do
          let (sresp, _) =
                eval $
                func
                  synct
                  ss
                  SyncRequest
                    { syncRequestAdded = M.empty
                    , syncRequestSynced = sis
                    , syncRequestDeleted = S.empty
                    }
          syncResponseServerAdded sresp `shouldBe` (serverStoreItems ss `diffSet` sis)
  it "returns all remotely added items when no items are locally deleted" $
    forAllValid $ \synct ->
      forAllValid $ \ss ->
        forAllValid $ \sis ->
          forAllValid $ \ais -> do
            let (sresp, _) =
                  eval $
                  func
                    synct
                    ss
                    SyncRequest
                      { syncRequestAdded = ais
                      , syncRequestSynced = sis
                      , syncRequestDeleted = S.empty
                      }
            syncResponseServerAdded sresp `shouldBe` (serverStoreItems ss `diffSet` sis)
  it "returns all remotely added items that weren't deleted when no items are locally added" $
    forAllValid $ \synct ->
      forAllValid $ \ss ->
        forAllValid $ \sis ->
          forAllValid $ \dis -> do
            let (sresp, _) =
                  eval $
                  func
                    synct
                    ss
                    SyncRequest
                      { syncRequestAdded = M.empty
                      , syncRequestSynced = sis
                      , syncRequestDeleted = dis
                      }
            syncResponseServerAdded sresp `shouldBe`
              (serverStoreItems ss `diffSet` sis `diffSet` dis)
  it "returns all remotely added items that weren't deleted" $
    forAllValid $ \synct ->
      forAllValid $ \ss ->
        forAllValid $ \sreq -> do
          let (sresp, _) = eval $ func synct ss sreq
          syncResponseServerAdded sresp `shouldBe`
            (serverStoreItems ss `diffSet`
             (syncRequestDeleted sreq `S.union` syncRequestSynced sreq))
  it "produces valid results" $ producesValidsOnValids3 $ \synct cs sr -> eval $ func synct cs sr
  it "successfully syncs two clients using a central store" $
    forAllValid $ \store1 ->
      forAllValid $ \(synct1, synct2, synct3) -> do
        let (s1, s2) =
              eval $ do
                let central = ServerStore M.empty
                let store2 = emptyClientStore
                let sreq1 = makeSyncRequest store1
                (sresp1, central') <- func synct1 central sreq1
                let store1' = mergeSyncResponse store1 sresp1
                let sreq2 = makeSyncRequest store2
                (sresp2, central'') <- func synct2 central' sreq2
                let store2' = mergeSyncResponse store2 sresp2
                let sreq3 = makeSyncRequest store1'
                (sresp3, _) <- func synct3 central'' sreq3
                let store1'' = mergeSyncResponse store1' sresp3
                pure (store1'', store2')
        s1 `shouldBe` s2
  it "ensures that syncing is idempotent" $
    forAllValid $ \synct1 ->
      forAll (genValid `suchThat` (>= synct1)) $ \synct2 ->
        forAllValid $ \central1 ->
          forAllValid $ \local1 -> do
            let ((local2, local3), (central2, central3)) =
                  eval $ do
                    let sreq1 = makeSyncRequest local1
                    (sresp1, central2) <- func synct1 central1 sreq1
                    let local2 = mergeSyncResponse local1 sresp1
                    let sreq2 = makeSyncRequest local2
                    (sresp2, central3) <- func synct2 central2 sreq2
                    let local3 = mergeSyncResponse local2 sresp2
                    pure ((local2, local3), (central2, central3))
            local2 `shouldBe` local3
            central2 `shouldBe` central3

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

diffSet :: Ord i => Map i a -> Set i -> Map i a
diffSet m s = m `M.difference` toMap s

toMap :: Set i -> Map i ()
toMap = M.fromSet (const ())
