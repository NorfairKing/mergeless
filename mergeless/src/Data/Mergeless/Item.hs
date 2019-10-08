{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeless.Item where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

{-# ANN module ("HLint: ignore Use lambda-case" :: String) #-}

-- | A local item of type @a@ that has been added but not synchronised yet
data Added a =
  Added
    { addedValue :: !a
    , addedCreated :: !UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Added a)

instance FromJSON a => FromJSON (Added a) where
  parseJSON = withObject "Added" $ \o -> Added <$> o .: "value" <*> o .: "added"

instance ToJSON a => ToJSON (Added a) where
  toJSON Added {..} = object ["value" .= addedValue, "added" .= addedCreated]

data Synced a =
  Synced
    { syncedValue :: !a
    , syncedCreated :: !UTCTime
    , syncedSynced :: !UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Synced a)

instance FromJSON a => FromJSON (Synced a)

instance ToJSON a => ToJSON (Synced a)

data ClientItem a
  = ClientEmpty
  | ClientAdded !(Added a)
  | ClientSynced !(Synced a)
  | ClientDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ClientItem a)

instance FromJSON a => FromJSON (ClientItem a)

instance ToJSON a => ToJSON (ClientItem a)

-- | A synchronisation request for items with identifiers of type @i@ and values of type @a@
data SyncRequest a
  = SyncRequestPoll
  | SyncRequestNew !(Added a)
  | SyncRequestKnown
  | SyncRequestDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (SyncRequest a)

instance FromJSON a => FromJSON (SyncRequest a)

instance ToJSON a => ToJSON (SyncRequest a)

makeItemSyncRequest :: ClientItem a -> SyncRequest a
makeItemSyncRequest ci =
  case ci of
    ClientEmpty -> SyncRequestPoll
    ClientAdded a -> SyncRequestNew a
    ClientSynced _ -> SyncRequestKnown
    ClientDeleted -> SyncRequestDeleted

-- | A synchronisation response for items with identifiers of type @i@ and values of type @a@
data SyncResponse a
  = SyncResponseInSyncEmpty
  | SyncResponseInSyncFull
  | SyncResponseClientAdded UTCTime -- The time when it was synced
  | SyncResponseClientDeleted
  | SyncResponseServerAdded !(Synced a)
  | SyncResponseServerDeleted
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (SyncResponse a)

instance FromJSON a => FromJSON (SyncResponse a)

instance ToJSON a => ToJSON (SyncResponse a)

-- | Merge a synchronisation response back into a client-side store.
mergeItemSyncResponse :: ClientItem a -> SyncResponse a -> ClientItem a
mergeItemSyncResponse ci sr =
  let mismatch = ci
   in case ci of
        ClientEmpty ->
          case sr of
            SyncResponseInSyncEmpty -> ClientEmpty
            SyncResponseServerAdded s -> ClientSynced s
            _ -> mismatch
        ClientAdded a ->
          case sr of
            SyncResponseClientAdded t -> ClientSynced (addedToSynced t a)
            SyncResponseServerAdded s -> ClientSynced s
            -- For completeness sake.
            -- This can only happen if two clients make the item at the same time.
            -- In practice, with named items in a collection, this will never happen.
            _ -> mismatch
        ClientSynced s ->
          case sr of
            SyncResponseInSyncFull -> ci -- No change
            SyncResponseServerDeleted -> ClientEmpty
            _ -> mismatch
        ClientDeleted ->
          case sr of
            SyncResponseClientDeleted -> ClientEmpty
            _ -> mismatch

-- | An item in a central store with a value of type @a@
data ServerItem a
  = ServerItemEmpty
  | ServerItemFull !(Synced a)
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ServerItem a)

instance FromJSON a => FromJSON (ServerItem a)

instance ToJSON a => ToJSON (ServerItem a)

processServerItemSync :: UTCTime -> ServerItem a -> SyncRequest a -> (SyncResponse a, ServerItem a)
processServerItemSync now si sr =
  case si of
    ServerItemEmpty ->
      case sr of
        SyncRequestPoll
          -- Both the client and the server think the item is empty, fine.
         -> (SyncResponseInSyncEmpty, si)
        SyncRequestNew a
          -- The client has a new item and the server has space for it, add it.
         -> (SyncResponseClientAdded now, ServerItemFull $ addedToSynced now a)
        SyncRequestKnown
          -- The client has an item that the server doesn't, so the server must have
          -- deleted it when another client asked to do that.
          -- Leave it deleted.
         -> (SyncResponseServerDeleted, si)
        SyncRequestDeleted
          -- The server has deleted an item but the current client hasn't been made aware of that
          -- AND this server also deleted that item in the meantime.
          -- Just leave it deleted.
         -> (SyncResponseClientDeleted, si)
    ServerItemFull s ->
      case sr of
        SyncRequestPoll
          -- The server has an item that the client doesn't, send it to the client.
         -> (SyncResponseServerAdded s, si)
        SyncRequestNew _
          -- The client wants to add an item that the server already has.
          -- That means that another client has added that same item in the meantime.
          -- This wouldn't happen if the items were named.
          -- In this case, for completeness sake,
         -> (SyncResponseServerAdded s, si)
        SyncRequestKnown -> (SyncResponseInSyncFull, si)
        SyncRequestDeleted -> (SyncResponseClientDeleted, ServerItemEmpty)

addedToSynced :: UTCTime -> Added a -> Synced a
addedToSynced syncTime Added {..} =
  Synced {syncedValue = addedValue, syncedCreated = addedCreated, syncedSynced = syncTime}
