{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

import Text.Hamlet
import Text.Blaze.Renderer.Text

import Web.Scotty
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import Database.Persist.Sqlite hiding (get)
import Text.Blaze.Internal
import Data.Maybe

import Manager
import Db

main :: IO ()
main = scotty 3000 $ do
     get "/" $ do
         rlogs <- liftIO $ (withSqliteConn "foo.db" . runSqlConn :: SqlPersist IO a -> IO a) $ do
           runMigration migrateAll
           processLog "log/development.log"
           rlogs <- selectList [] []
           mapM getActionById rlogs
         html $ renderMarkup $ template (rlogs :: [[T.Text]]) ""
           where
             template :: [[T.Text]] -> T.Text -> Markup
             template tss = [hamlet|
                            $doctype 5
                            <html>
                              <body>
                                <table>
                                  $forall ts <- tss
                                    <tr>
                                      $forall t <- ts
                                        <td>#{t}
                            |]

             getActionById :: Entity RLog -> SqlPersist IO [T.Text]
             getActionById rlog = do
               first <- selectFirst [RActionId ==. (rLogActionId $ entityVal rlog)] []
               case first of
                 Just v -> do
                   let v' = entityVal v
                   return $ (map (T.pack . fromMaybe "(nil)") $ [rActionController v', rActionAction v']) ++ [T.pack $ rLogContent $ entityVal $ rlog]
                 Nothing -> return []
