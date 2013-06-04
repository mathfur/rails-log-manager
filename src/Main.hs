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
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Manager
import Db

main :: IO ()
main = scotty 3000 $ do
     middleware logStdoutDev
     middleware $ staticPolicy $ addBase "static"
     get "/" $ do
         q <- param "q"
         rlogs <- liftIO $ (withSqliteConn "foo.db" . runSqlConn :: SqlPersist IO a -> IO a) $ do
           runMigration migrateAll
           processLog "log/development.log"
           rlogs <- selectList [] []
           mapM getActionById $ filter (haveTextInContent q) rlogs
         html $ renderMarkup $ template (rlogs :: [[T.Text]]) ""
           where
             template :: [[T.Text]] -> T.Text -> Markup
             template tss = [hamlet|
                            $doctype 5
                            <html>
                              <head>
                                <title>Rails Log Manager
                                <script src="js/bootstrap.js">
                                <link href="css/bootstrap.css" rel="stylesheet" type="text/css">
                                <link href="css/bootstrap-responsive.css" rel="stylesheet" type="text/css">
                              <body>
                                <div class="navbar navbar-fixed-top">
                                  <div class="navbar-inner">
                                    <div class="container">
                                      <ul class="nav pull-left">
                                        <a class="brand">Rails-Log-Manager
                                <table class="table table-striped table-bordered table-hover">
                                  $forall ts <- tss
                                    <tr>
                                      $forall t <- ts
                                        <td>#{t}
                            |]

             haveTextInContent :: T.Text -> Entity RLog -> Bool
             haveTextInContent q rlog = T.isInfixOf q ((T.pack . rLogContent . entityVal) rlog)

             getActionById :: Entity RLog -> SqlPersist IO [T.Text]
             getActionById rlog = do
               first <- selectFirst [RActionId ==. (rLogActionId $ entityVal rlog)] []
               case first of
                 Just v -> do
                   let v' = entityVal v
                   return $ (map (T.pack . fromMaybe "(nil)") $
                              [
                                rActionController v',
                                rActionAction v',
                                rActionMethod v',
                                rActionResponse v',
                                rActionDatetime v'
                              ]
                            ) ++ [T.pack $ rLogContent $ entityVal $ rlog]
                 Nothing -> return []
