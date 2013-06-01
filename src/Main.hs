{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Web.Scotty
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import Database.Persist.Sqlite hiding (get)
import Control.Applicative

import Manager
import Db

main :: IO ()
main = scotty 3000 $ do
     get "/" $ do
         rc <- liftIO $ (withSqliteConn "foo.db" . runSqlConn :: SqlPersist IO a -> IO a) $ do
           runMigration migrateAll
           processLog "log/development.log"
           map (rLogContent . entityVal) <$> selectList [] []
         html $ T.pack ("<table>" ++ (concat $ map (\e -> "<tr><td>" ++ e ++ "</td></tr>") rc) ++ "</table>")
