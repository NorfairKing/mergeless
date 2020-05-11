{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Data.GenValidity
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateServer"]
  [persistLowerCase|

ServerThing
  number Int

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ServerThing

instance GenUnchecked ServerThing

instance GenValid ServerThing
