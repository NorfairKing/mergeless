{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Mergeless.Persistent where

import Data.Mergeless
import Database.Persist
import Database.Persist.Sql

deriving instance PersistFieldSql ClientId

deriving instance PersistField ClientId
-- FIXME: There is a constraint that clientId and serverId can't both be Nothing
-- This isn't nice but we can maybe fix it with a singe data type that is serialised as Word64 in both cases (somehow)
-- Or maybe we can convert the server side id field into a ClientId!!
