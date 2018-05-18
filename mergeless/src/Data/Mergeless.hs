{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Mergeless
    ( Store(..)
    , StoreItem(..)
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , makeSyncRequest
    , SyncResponse(..)
    , mergeSyncResponse
    ) where

import Control.Applicative
import Data.Aeson
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Store a = Store
    { storeItems :: Set (StoreItem a)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (Store a) where
    validate Store {..} =
        mconcat
            [ annotate storeItems "storeItems"
            , declare "the store items have distinct uuids" $
              distinct $
              flip mapMaybe (S.toList storeItems) $ \si ->
                  case si of
                      UnsyncedItem _ -> Nothing
                      SyncedItem Synced {..} -> Just syncedUuid
                      UndeletedItem u -> Just u
            ]

distinct :: Ord a => [a] -> Bool
distinct ls = sort ls == S.toAscList (S.fromList ls)

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

data SyncRequest a = SyncRequest
    { syncRequestAddedItems :: Set (Added a)
    , syncRequestSyncedItems :: Set (UUID a)
    , syncRequestUndeletedItems :: Set (UUID a)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (SyncRequest a) where
    validate SyncRequest {..} =
        mconcat
            [ annotate syncRequestAddedItems "syncRequestAddedItems"
            , annotate syncRequestSyncedItems "syncRequestSyncedItems"
            , annotate syncRequestUndeletedItems "syncRequestUndeletedItems"
            , declare "the sync request items have distinct uuids" $
              distinct $
              S.toList syncRequestSyncedItems ++
              S.toList syncRequestUndeletedItems
            ]

instance (FromJSON a, Ord a) => FromJSON (SyncRequest a) where
    parseJSON =
        withObject "SyncRequest" $ \o ->
            SyncRequest <$> o .: "unsynced" <*> o .: "synced" <*>
            o .: "undeleted"

instance ToJSON a => ToJSON (SyncRequest a) where
    toJSON SyncRequest {..} =
        object
            [ "unsynced" .= syncRequestAddedItems
            , "synced" .= syncRequestSyncedItems
            , "undeleted" .= syncRequestUndeletedItems
            ]

data SyncResponse a = SyncResponse
    { syncResponseAddedItems :: Set (Synced a)
    , syncResponseNewRemoteItems :: Set (Synced a)
    , syncResponseItemsToBeDeletedLocally :: Set (UUID a)
    } deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (SyncResponse a) where
    validate SyncResponse {..} =
        mconcat
            [ annotate syncResponseAddedItems "syncResponseAddedItems"
            , annotate syncResponseNewRemoteItems "syncResponseNewRemoteItems"
            , annotate
                  syncResponseItemsToBeDeletedLocally
                  "syncResponseItemsToBeDeletedLocally"
            , declare "the sync response items have distinct uuids" $
              distinct $
              map
                  syncedUuid
                  (S.toList syncResponseAddedItems ++
                   S.toList syncResponseNewRemoteItems) ++
              S.toList syncResponseItemsToBeDeletedLocally
            ]

instance (FromJSON a, Ord a) => FromJSON (SyncResponse a) where
    parseJSON =
        withObject "SyncResponse" $ \o ->
            SyncResponse <$> o .: "added" <*> o .: "new" <*> o .: "deleted"

instance ToJSON a => ToJSON (SyncResponse a) where
    toJSON SyncResponse {..} =
        object
            [ "added" .= syncResponseAddedItems
            , "new" .= syncResponseNewRemoteItems
            , "deleted" .= syncResponseItemsToBeDeletedLocally
            ]

makeSyncRequest :: Ord a => Store a -> SyncRequest a
makeSyncRequest Store {..} =
    SyncRequest
    { syncRequestAddedItems =
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  UnsyncedItem a -> Just a
                  _ -> Nothing
    , syncRequestSyncedItems =
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  SyncedItem i -> Just $ syncedUuid i
                  _ -> Nothing
    , syncRequestUndeletedItems =
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  UndeletedItem uuid -> Just uuid
                  _ -> Nothing
    }

mergeSyncResponse :: Ord a => Store a -> SyncResponse a -> Store a
mergeSyncResponse s SyncResponse {..} =
    let withNewOwnItems =
            flip mapSetMaybe (storeItems s) $ \si ->
                case si of
                    UnsyncedItem Added {..} ->
                        case find
                                 (\Synced {..} ->
                                      and
                                          [ syncedCreated == addedAdded
                                          , syncedValue == addedValue
                                          ])
                                 syncResponseAddedItems of
                            Nothing -> Just si -- If it wasn't added (for whatever reason), just leave it as unsynced
                            Just ii -> Just $ SyncedItem ii -- If it was added, then it becomes synced
                    SyncedItem ii ->
                        case find
                                 (== syncedUuid ii)
                                 syncResponseItemsToBeDeletedLocally of
                            Nothing -> Just si -- If it wasn't deleted, don't delete it.
                            Just _ -> Nothing -- If it was deleted, delete it here.
                    UndeletedItem _ -> Nothing -- Delete all locally deleted items after sync
    in Store
       { storeItems =
             S.fromList .
             nubBy
                 (\i1 i2 ->
                      case (i1, i2) of
                          (UnsyncedItem _, _) -> False
                          (_, UnsyncedItem _) -> False
                          (SyncedItem s1, SyncedItem s2) ->
                              syncedUuid s1 == syncedUuid s2
                          (SyncedItem s1, UndeletedItem u2) ->
                              syncedUuid s1 == u2
                          (UndeletedItem u1, SyncedItem  s2) ->
                              u1 == syncedUuid s2
                          (UndeletedItem u1, UndeletedItem u2) -> u1 == u2) .
             S.toList $
             S.map SyncedItem syncResponseNewRemoteItems `S.union`
             withNewOwnItems
       }

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func
