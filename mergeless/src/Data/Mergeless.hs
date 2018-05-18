{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Mergeless
    ( Store(..)
    , StoreItem(..)
    , Added(..)
    , Synced(..)
    ) where

import Control.Applicative
import Data.Aeson
import Data.Set as S
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Store a = Store
    { storeItems :: Set (StoreItem a)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (Store a)

instance (FromJSON a, Ord a) => FromJSON (Store a) where
    parseJSON v = Store <$> parseJSON v

instance ToJSON a => ToJSON (Store a) where
    toJSON (Store s) = toJSON s

data StoreItem a
    = UnsyncedItem (Added a)
    | SyncedItem (Synced a)
    | UndeletedItem (UUID a)
    deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (StoreItem a)

instance FromJSON a => FromJSON (StoreItem a) where
    parseJSON v =
        (SyncedItem <$> parseJSON v) <|> (UnsyncedItem <$> parseJSON v) <|>
        (UndeletedItem <$> parseJSON v)

instance ToJSON a => ToJSON (StoreItem a) where
    toJSON (UnsyncedItem a) = toJSON a
    toJSON (SyncedItem a) = toJSON a
    toJSON (UndeletedItem a) = toJSON a

data Added a = Added
    { addedValue :: a
    , addedAdded :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Added a)

instance FromJSON a => FromJSON (Added a) where
    parseJSON =
        withObject "Added" $ \o -> Added <$> o .: "value" <*> o .: "added"

instance ToJSON a => ToJSON (Added a) where
    toJSON Added {..} = object ["value" .= addedValue, "added" .= addedAdded]

data Synced a = Synced
    { syncedUuid :: UUID a
    , syncedValue :: a
    , syncedCreated :: UTCTime
    , syncedSynced :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Synced a)

instance FromJSON a => FromJSON (Synced a) where
    parseJSON =
        withObject "Synced" $ \o ->
            Synced <$> o .: "uuid" <*> o .: "value" <*> o .: "created" <*>
            o .: "synced"

instance ToJSON a => ToJSON (Synced a) where
    toJSON Synced {..} =
        object
            [ "uuid" .= syncedUuid
            , "value" .= syncedValue
            , "created" .= syncedCreated
            , "synced" .= syncedSynced
            ]
