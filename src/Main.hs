{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Web.Scotty
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T

import Manager
import Db

main = scotty 3000 $ do
     get "/" $ do
         rs <- liftIO readFromDb'
         html $ T.pack ("<table>" ++ (concat $ map (\e -> "<tr><td>" ++ e ++ "</td></tr>") rs) ++ "</table>")

     post "/flush" $ do
         liftIO $ parseLog "log/development.log"
         html ""
