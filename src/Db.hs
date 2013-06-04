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
RAction
    controller String Maybe
    action String Maybe
    ip String Maybe
    datetime String Maybe
    method String Maybe
    parameters String Maybe
    time String Maybe
    view_time String Maybe
    db_time String Maybe
    response String Maybe
    url String Maybe
    deriving Show
RLog
    content String
    actionId RActionId
    deriving Show
LogState
    side Side
    pos Int
    stack [String]
    deriving Show
|]
