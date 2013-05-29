{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

module Db where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

import Type

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
|]

saveToDb :: RailsLog -> IO ()
saveToDb rails_log = withSqliteConn "foo.db" $ (runSqlConn :: SqlPersist IO a -> Connection -> IO a) $ do
    runMigration migrateAll
    actionId <- insert $ Action (lookup "controller" rails_log) (lookup "action" rails_log) (lookup "method" rails_log) (lookup "code" rails_log) (lookup "time" rails_log)
    mapM_ insert $ map (((flip RLog) actionId) . snd) $ filter ((flip (==) "other") . fst) rails_log

readFromDb :: IO [Entity RLog]
readFromDb = withSqliteConn "foo.db" $ (runSqlConn :: SqlPersist IO a -> Connection -> IO a) $ selectList [] []

readFromDb' :: IO [String]
readFromDb' = readFromDb >>= (return . map (rLogContent . entityVal))
