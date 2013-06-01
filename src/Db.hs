{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

module Db where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Store

import Type

instance PersistField Side where
    toPersistValue Inside = PersistBool True
    toPersistValue Outside = PersistBool False
    fromPersistValue (PersistBool True) = Right Inside
    fromPersistValue _ = Right Outside
    sqlType _ = SqlBool
    isNullable _ = False

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Action
    controller String Maybe
    action String Maybe
    method String Maybe
    code String Maybe
    time String Maybe
    deriving Show
RLog
    content String
    actionId ActionId
    deriving Show
LogState
    side Side
    stack [String]
    deriving Show
|]
