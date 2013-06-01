{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Manager where

import Data.Maybe
import Control.Monad.State
import Control.Applicative
import Database.Persist.Sqlite hiding (get)

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
            put $ LogState (logStateSide s) (logStateStack s ++ [newline])

        clear :: RailsLogManager
        clear = do
            s <- get
            put $ LogState (logStateSide s) []

        moveTo :: Side -> RailsLogManager
        moveTo l = (put . LogState l . logStateStack) =<< get

        parseAndOutput :: RailsLogManager
        parseAndOutput = (lift . saveToDb . convertToRailsLog . logStateStack) =<< get
          where
            convertToRailsLog :: [String] -> RailsLog
            convertToRailsLog = concat . fmap parseLogLine

            saveToDb :: RailsLog -> SqlPersist IO ()
            saveToDb rails_log = do
                actionId <- insert $ Action (lookup "controller" rails_log) (lookup "action" rails_log) (lookup "method" rails_log) (lookup "code" rails_log) (lookup "time" rails_log)
                mapM_ insert $ map (((flip RLog) actionId) . snd) $ filter ((flip (==) "other") . fst) rails_log

------------------------------------------------------

processLines :: FilePath -> RailsLogManager
processLines = (liftIO . readFile) >=> (mapM_ processLine . lines)

processLog :: FilePath -> SqlPersist IO ()
processLog fname = loadState >>= (execStateT $ processLines fname) >>= saveState
  where
    loadState :: SqlPersist IO LogState
    loadState = do
      (fromMaybe startPos . listToMaybe . map entityVal) <$> selectList [] []
        where
          startPos :: LogState
          startPos = LogState Outside []

    saveState :: LogState -> SqlPersist IO ()
    saveState s = insert s >> return ()
