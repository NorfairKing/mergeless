{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
        "Item"
        [ genValidBench @(ClientItem Bool),
          genValidBench @(ItemSyncRequest Bool),
          genValidBench @(ItemSyncResponse Bool),
          genValidBench @(ServerItem Bool)
        ],
      bgroup
        "Collection"
        [ genValidBench @ClientId,
          genValidBench @(ClientStore ClientId Int Bool),
          genValidBench @(SyncRequest ClientId Int Bool),
          genValidBench @(SyncResponse ClientId Int Bool),
          genValidBench @(ServerStore Int Bool)
        ]
    ]
