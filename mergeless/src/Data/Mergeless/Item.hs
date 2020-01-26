{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeless.Item where

import Control.DeepSeq
import Data.Aeson
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

data ClientItem a
  = ClientEmpty
  | ClientAdded !a
  | ClientSynced !a
  | ClientDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ClientItem a)

instance NFData a => NFData (ClientItem a)

instance FromJSON a => FromJSON (ClientItem a)

instance ToJSON a => ToJSON (ClientItem a)

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data ItemSyncRequest a
  = ItemSyncRequestPoll
  | ItemSyncRequestNew a
  | ItemSyncRequestKnown
  | ItemSyncRequestDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ItemSyncRequest a)

instance NFData a => NFData (ItemSyncRequest a)

instance FromJSON a => FromJSON (ItemSyncRequest a)

instance ToJSON a => ToJSON (ItemSyncRequest a)

makeItemSyncRequest :: ClientItem a -> ItemSyncRequest a
makeItemSyncRequest ci =
  case ci of
    ClientEmpty -> ItemSyncRequestPoll
    ClientAdded a -> ItemSyncRequestNew a
    ClientSynced _ -> ItemSyncRequestKnown
    ClientDeleted -> ItemSyncRequestDeleted

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data ItemSyncResponse a
  = ItemSyncResponseInSyncEmpty
  | ItemSyncResponseInSyncFull
  | ItemSyncResponseClientAdded
  | ItemSyncResponseClientDeleted
  | ItemSyncResponseServerAdded !a
  | ItemSyncResponseServerDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ItemSyncResponse a)

instance NFData a => NFData (ItemSyncResponse a)

instance FromJSON a => FromJSON (ItemSyncResponse a)

instance ToJSON a => ToJSON (ItemSyncResponse a)

-- | Merge a synchronisation response back into a client-side store.
mergeItemSyncResponse :: ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponse ci sr =
  let mismatch = ci
   in case ci of
        ClientEmpty ->
          case sr of
            ItemSyncResponseInSyncEmpty -> ClientEmpty
            ItemSyncResponseServerAdded s -> ClientSynced s
            _ -> mismatch
        ClientAdded a ->
          case sr of
            ItemSyncResponseClientAdded -> ClientSynced a
            ItemSyncResponseServerAdded s -> ClientSynced s
            -- For completeness sake.
            -- This can only happen if two clients make the item at the same time.
            -- In practice, with named items in a collection, this will never happen.
            _ -> mismatch
        ClientSynced _ ->
          case sr of
            ItemSyncResponseInSyncFull -> ci -- No change
            ItemSyncResponseServerDeleted -> ClientEmpty
            _ -> mismatch
        ClientDeleted ->
          case sr of
            ItemSyncResponseClientDeleted -> ClientEmpty
            _ -> mismatch

-- | An item in a central store with a value of type @a@
data ServerItem a
  = ServerItemEmpty
  | ServerItemFull !a
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ServerItem a)

instance NFData a => NFData (ServerItem a)

instance FromJSON a => FromJSON (ServerItem a)

instance ToJSON a => ToJSON (ServerItem a)

processServerItemSync :: ServerItem a -> ItemSyncRequest a -> (ItemSyncResponse a, ServerItem a)
processServerItemSync si sr =
  case si of
    ServerItemEmpty ->
      case sr of
        ItemSyncRequestPoll
          -- Both the client and the server think the item is empty, fine.
         -> (ItemSyncResponseInSyncEmpty, si)
        ItemSyncRequestNew a
          -- The client has a new item and the server has space for it, add it.
         -> (ItemSyncResponseClientAdded, ServerItemFull a)
        ItemSyncRequestKnown
          -- The client has an item that the server doesn't, so the server must have
          -- deleted it when another client asked to do that.
          -- Leave it deleted.
         -> (ItemSyncResponseServerDeleted, si)
        ItemSyncRequestDeleted
          -- The server has deleted an item but the current client hasn't been made aware of that
          -- AND this server also deleted that item in the meantime.
          -- Just leave it deleted.
         -> (ItemSyncResponseClientDeleted, si)
    ServerItemFull s ->
      case sr of
        ItemSyncRequestPoll
          -- The server has an item that the client doesn't, send it to the client.
         -> (ItemSyncResponseServerAdded s, si)
        ItemSyncRequestNew _
          -- The client wants to add an item that the server already has.
          -- That means that another client has added that same item in the meantime.
          -- This wouldn't happen if the items were named.
          -- In this case, for completeness sake,
         -> (ItemSyncResponseServerAdded s, si)
        ItemSyncRequestKnown -> (ItemSyncResponseInSyncFull, si)
        ItemSyncRequestDeleted -> (ItemSyncResponseClientDeleted, ServerItemEmpty)
