{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main as Criterion

import Data.GenValidity.Criterion
import Data.GenValidity.Mergeless ()

import Data.Mergeless.Collection
import Data.Mergeless.Item

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "Utils"
        [ genValidBench @(Added Bool)
        , genValidBench @(Synced Bool)
        , genValidBench @(ClientAddition Int)
        ]
    , bgroup
        "Item"
        [ genValidBench @(ClientItem Bool)
        , genValidBench @(ItemSyncRequest Bool)
        , genValidBench @(ItemSyncResponse Bool)
        , genValidBench @(ServerItem Bool)
        ]
    , bgroup
        "Collection"
        [ genValidBench @ClientId
        , genValidBench @(ClientStore Int Bool)
        , genValidBench @(SyncRequest Int Bool)
        , genValidBench @(ClientAddition Int)
        , genValidBench @(SyncResponse Int Bool)
        , genValidBench @(ServerStore Int Bool)
        ]
    ]
