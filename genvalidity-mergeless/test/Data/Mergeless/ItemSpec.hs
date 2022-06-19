{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeless.ItemSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Data.Data
import Data.GenValidity.Mergeless.Item ()
import Data.GenValidity.UUID ()
import Data.Mergeless.Item
import Data.Word
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils
import Text.Colour

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  let yamlSchemaSpec :: forall a. (Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
      yamlSchemaSpec filePath = do
        it ("outputs the same schema as before for " <> nameOf @a) $
          pureGoldenByteStringFile
            ("test_resources/item/" <> filePath <> ".txt")
            (renderChunksBS With24BitColours $ schemaChunksViaCodec @a)

  describe "ClientItem" $ do
    genValidSpec @(ClientItem Word8)
    jsonSpec @(ClientItem Word8)
    yamlSchemaSpec @(ClientItem Word8) "client"
  describe "ItemSyncRequest" $ do
    genValidSpec @(ItemSyncRequest Word8)
    jsonSpec @(ItemSyncRequest Word8)
    yamlSchemaSpec @(ClientItem Word8) "request"
  describe "ItemSyncResponse" $ do
    genValidSpec @(ItemSyncResponse Word8)
    jsonSpec @(ItemSyncResponse Word8)
    yamlSchemaSpec @(ClientItem Word8) "response"
  describe "ServerItem" $ do
    genValidSpec @(ServerItem Word8)
    jsonSpec @(ServerItem Word8)
    yamlSchemaSpec @(ClientItem Word8) "server"
  describe "makeItemSyncrequest" $
    it "produces valid requests" $
      producesValid (makeItemSyncRequest @Int)
  describe "mergeItemSyncResponse" $
    it "produces valid client items" $
      producesValid2 (mergeItemSyncResponse @Int)
  describe "processItemSync" $
    it "produces valid tuples" $
      producesValid2 (processServerItemSync @Int)
  describe "processItemSync" $ do
    describe "Interaction" $ do
      it "makes no changes if the sync request reflects the state of the empty server" $ do
        let store1 = ServerItemEmpty
            req = ItemSyncRequestPoll
        let (resp, store2) = processServerItemSync @Int store1 req
        store2 `shouldBe` store1
        resp `shouldBe` ItemSyncResponseInSyncEmpty
      it "makes no changes if the sync request reflects the state of the full server" $
        forAllValid $
          \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestKnown
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseInSyncFull
    describe "Client changes" $ do
      it "adds the item that the client tells the server to add" $
        forAllValid $
          \a -> do
            let store1 = ServerItemEmpty
                req = ItemSyncRequestNew a
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` ServerItemFull a
            resp `shouldBe` ItemSyncResponseClientAdded
      it "deletes the item that the client tells the server to delete" $
        forAllValid $
          \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestDeleted
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` ServerItemEmpty
            resp `shouldBe` ItemSyncResponseClientDeleted
    describe "Server changes" $ do
      it "tells the client that there is a new item at the server side" $
        forAllValid $
          \s -> do
            let store1 = ServerItemFull s
                req = ItemSyncRequestPoll
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseServerAdded s
      it "tells the client that there is a deleted item at the server side" $ do
        let store1 = ServerItemEmpty
            req = ItemSyncRequestKnown
        let (resp, store2) = processServerItemSync @Int store1 req
        store2 `shouldBe` store1
        resp `shouldBe` ItemSyncResponseServerDeleted
    describe "High level properties" $ do
      it "it always possible to add an item from scratch" $
        forAllValid $
          \a -> do
            let cstore1 = ClientAdded (a :: Int)
            let sstore1 = ServerItemEmpty
            let req1 = makeItemSyncRequest cstore1
                (resp1, sstore2) = processServerItemSync sstore1 req1
                cstore2 = mergeItemSyncResponse cstore1 resp1
            resp1 `shouldBe` ItemSyncResponseClientAdded
            sstore2 `shouldBe` ServerItemFull a
            cstore2 `shouldBe` ClientSynced a
      it "is idempotent with one client" $
        forAllValid $
          \si1 ->
            forAllValid $ \ci1 -> do
              let req1 = makeItemSyncRequest @Int ci1
                  (resp1, si2) = processServerItemSync si1 req1
                  ci2 = mergeItemSyncResponse ci1 resp1
                  req2 = makeItemSyncRequest ci2
                  (resp2, si3) = processServerItemSync si2 req2
                  ci3 = mergeItemSyncResponse ci2 resp2
              ci3 `shouldBe` ci2
              si3 `shouldBe` si2
