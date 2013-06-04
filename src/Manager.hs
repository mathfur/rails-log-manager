{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Manager where

import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Database.Persist.Sqlite hiding (get)
import System.IO

import Parser
import Type
import Db

type RailsLogManager = StateT LogState (SqlPersist IO) ()

processLine :: String -> RailsLogManager
processLine line = do
    s <- get
    case (logStateSide s) of
        Outside -> case (inputKind line) of
                  Start -> clear >> (push line) >> moveTo Inside
                  _     -> return ()
        Inside -> case (inputKind line) of
                  Start -> parseAndOutput >> clear >> (push line)
                  End   -> (push line) >> parseAndOutput >> clear >> moveTo Outside
                  _     -> (push line)
      where
        push :: String -> RailsLogManager
        push newline = do
            s <- get
            put $ LogState (logStateSide s) (logStatePos s) (logStateStack s ++ [newline])

        clear :: RailsLogManager
        clear = do
            s <- get
            put $ LogState (logStateSide s) (logStatePos s) []

        moveTo :: Side -> RailsLogManager
        moveTo l = do
            s <- get
            put $ LogState l (logStatePos s) (logStateStack s)

        parseAndOutput :: RailsLogManager
        parseAndOutput = (lift . saveToDb . convertToRailsLog . logStateStack) =<< get
          where
            convertToRailsLog :: [String] -> RailsLog
            convertToRailsLog = concat . fmap parseLogLine

            saveToDb :: RailsLog -> SqlPersist IO ()
            saveToDb rails_log = do
                actionId <- insert $ RAction
                                       (lookup "controller" rails_log)
                                       (lookup "action" rails_log)
                                       (lookup "ip" rails_log)
                                       (lookup "datetime" rails_log)
                                       (lookup "method" rails_log)
                                       (lookup "parameters" rails_log)
                                       (lookup "time" rails_log)
                                       (lookup "view_time" rails_log)
                                       (lookup "db_time" rails_log)
                                       (lookup "response" rails_log)
                                       (lookup "url" rails_log)
                mapM_ insert $ map (((flip RLog) actionId) . snd) $ filter ((flip (==) "other") . fst) rails_log
------------------------------------------------------

processLines :: FilePath -> RailsLogManager
processLines path = do
    s <- get
    cnt <- liftIO $ do
      handle <- openFile path ReadMode
      hSeek handle AbsoluteSeek $ toInteger $ logStatePos s
      hGetContents handle
    mapM_ processLine $ lines cnt
    incPos $ length cnt
    return ()
  where
    incPos :: Int -> RailsLogManager
    incPos pos = do
        s <- get
        put $ LogState (logStateSide s) ((logStatePos s) + pos) (logStateStack s)

processLog :: FilePath -> SqlPersist IO ()
processLog fname = loadState >>= (execStateT $ processLines fname) >>= saveState
  where
    loadState :: SqlPersist IO LogState
    loadState = do
      arr <- selectList [] []
      mapM_ delete $ map entityKey arr
      return $ (fromMaybe startPos . listToMaybe . map entityVal) arr
        where
          startPos :: LogState
          startPos = LogState Outside 0 []

    saveState :: LogState -> SqlPersist IO ()
    saveState s = insert s >> return ()
