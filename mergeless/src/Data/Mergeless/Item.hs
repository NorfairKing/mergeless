{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeless.Item where

import Autodocodec
import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

data ClientItem a
  = ClientEmpty
  | ClientAdded !a
  | ClientSynced !a
  | ClientDeleted
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientItem a))

instance (Validity a) => Validity (ClientItem a)

instance (NFData a) => NFData (ClientItem a)

instance (HasCodec a) => HasCodec (ClientItem a) where
  codec =
    object "ClientItem" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "empty" <*> pure ())
              (typeField "added" <*> requiredField "value" "item that was added, client-side")
          )
          ( disjointEitherCodec
              (typeField "synced" <*> requiredField "value" "the item that is known, client-side")
              (typeField "deleted" <*> pure ())
          )
    where
      f :: Either (Either () a) (Either a ()) -> ClientItem a
      f = \case
        Left (Left ()) -> ClientEmpty
        Left (Right v) -> ClientAdded v
        Right (Left v) -> ClientSynced v
        Right (Right ()) -> ClientDeleted
      g :: ClientItem a -> Either (Either () a) (Either a ())
      g = \case
        ClientEmpty -> Left (Left ())
        ClientAdded v -> Left (Right v)
        ClientSynced v -> Right (Left v)
        ClientDeleted -> Right (Right ())

      typeField :: Text -> ObjectCodec b (x -> x)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data ItemSyncRequest a
  = ItemSyncRequestPoll
  | ItemSyncRequestNew !a
  | ItemSyncRequestKnown
  | ItemSyncRequestDeleted
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemSyncRequest a))

instance (Validity a) => Validity (ItemSyncRequest a)

instance (NFData a) => NFData (ItemSyncRequest a)

instance (HasCodec a) => HasCodec (ItemSyncRequest a) where
  codec =
    object "ItemSyncRequest" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "empty" <*> pure ())
              (typeField "added" <*> requiredField "value" "item that was added, client-side")
          )
          ( disjointEitherCodec
              (typeField "synced" <*> pure ())
              (typeField "deleted" <*> pure ())
          )
    where
      f = \case
        Left (Left ()) -> ItemSyncRequestPoll
        Left (Right v) -> ItemSyncRequestNew v
        Right (Left ()) -> ItemSyncRequestKnown
        Right (Right ()) -> ItemSyncRequestDeleted

      g = \case
        ItemSyncRequestPoll -> Left (Left ())
        ItemSyncRequestNew v -> Left (Right v)
        ItemSyncRequestKnown -> Right (Left ())
        ItemSyncRequestDeleted -> Right (Right ())

      typeField :: Text -> ObjectCodec b (x -> x)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemSyncResponse a))

instance (Validity a) => Validity (ItemSyncResponse a)

instance (NFData a) => NFData (ItemSyncResponse a)

instance (HasCodec a) => HasCodec (ItemSyncResponse a) where
  codec =
    object "ItemSyncResponse" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "in-sync-empty" <*> pure ())
              (typeField "in-sync-full" <*> pure ())
          )
          ( disjointEitherCodec
              ( disjointEitherCodec
                  (typeField "client-added" <*> pure ())
                  (typeField "client-deleted" <*> pure ())
              )
              ( disjointEitherCodec
                  (typeField "server-added" <*> requiredField "value" "the value that was added, server-side")
                  (typeField "server-deleted" <*> pure ())
              )
          )
    where
      f = \case
        Left (Left ()) -> ItemSyncResponseInSyncEmpty
        Left (Right ()) -> ItemSyncResponseInSyncFull
        Right (Left (Left ())) -> ItemSyncResponseClientAdded
        Right (Left (Right ())) -> ItemSyncResponseClientDeleted
        Right (Right (Left v)) -> ItemSyncResponseServerAdded v
        Right (Right (Right ())) -> ItemSyncResponseServerDeleted

      g = \case
        ItemSyncResponseInSyncEmpty -> Left (Left ())
        ItemSyncResponseInSyncFull -> Left (Right ())
        ItemSyncResponseClientAdded -> Right (Left (Left ()))
        ItemSyncResponseClientDeleted -> Right (Left (Right ()))
        ItemSyncResponseServerAdded v -> Right (Right (Left v))
        ItemSyncResponseServerDeleted -> Right (Right (Right ()))

      typeField :: Text -> ObjectCodec b (x -> x)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ServerItem a))

instance (Validity a) => Validity (ServerItem a)

instance (NFData a) => NFData (ServerItem a)

instance (HasCodec a) => HasCodec (ServerItem a) where
  codec =
    object "ServerItem" $
      dimapCodec f g $
        possiblyJointEitherCodec
          (requiredField "value" "the item on the server side")
          (pure ())
    where
      f = \case
        Left v -> ServerItemFull v
        Right () -> ServerItemEmpty
      g = \case
        ServerItemFull v -> Left v
        ServerItemEmpty -> Right ()

processServerItemSync :: ServerItem a -> ItemSyncRequest a -> (ItemSyncResponse a, ServerItem a)
processServerItemSync si sr =
  case si of
    ServerItemEmpty ->
      case sr of
        ItemSyncRequestPoll ->
          -- Both the client and the server think the item is empty, fine.
          (ItemSyncResponseInSyncEmpty, si)
        ItemSyncRequestNew a ->
          -- The client has a new item and the server has space for it, add it.
          (ItemSyncResponseClientAdded, ServerItemFull a)
        ItemSyncRequestKnown ->
          -- The client has an item that the server doesn't, so the server must have
          -- deleted it when another client asked to do that.
          -- Leave it deleted.
          (ItemSyncResponseServerDeleted, si)
        ItemSyncRequestDeleted ->
          -- The server has deleted an item but the current client hasn't been made aware of that
          -- AND this server also deleted that item in the meantime.
          -- Just leave it deleted.
          (ItemSyncResponseClientDeleted, si)
    ServerItemFull s ->
      case sr of
        ItemSyncRequestPoll ->
          -- The server has an item that the client doesn't, send it to the client.
          (ItemSyncResponseServerAdded s, si)
        ItemSyncRequestNew _ ->
          -- The client wants to add an item that the server already has.
          -- That means that another client has added that same item in the meantime.
          -- This wouldn't happen if the items were named.
          -- In this case, for completeness sake,
          (ItemSyncResponseServerAdded s, si)
        ItemSyncRequestKnown -> (ItemSyncResponseInSyncFull, si)
        ItemSyncRequestDeleted -> (ItemSyncResponseClientDeleted, ServerItemEmpty)
