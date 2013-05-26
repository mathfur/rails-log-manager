{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Manager where

import Control.Monad.State

import Parser
import Type

exec :: FilePath -> RailsLogManager
exec = (liftIO . readFile) >=> (mapM_ processLine . lines)

processLine :: String -> RailsLogManager
processLine line = do
    current_state <- get
    case (loc current_state) of
        Outside -> case (inputKind line) of
                  Start -> clear >> (push line) >> moveTo Inside
                  _     -> return ()
        Inside -> case (inputKind line) of
                  Start -> parse_and_output >> clear >> (push line)
                  End   -> (push line) >> parse_and_output >> clear >> moveTo Outside
                  _     -> (push line)

push :: String -> RailsLogManager
push line = do
    s <- get
    put $ LogState (loc s) (stack s ++ [line])

clear :: RailsLogManager
clear = do
    s <- get
    put $ LogState (loc s) []

moveTo :: LogLocation -> RailsLogManager
moveTo l = (put . LogState l . stack) =<< get

parse_and_output :: RailsLogManager
parse_and_output = (liftIO . save_to_db . convertToRailsLog . stack) =<< get

convertToRailsLog :: [String] -> RailsLog
convertToRailsLog = concat . fmap parseLogLine

save_to_db :: RailsLog -> IO ()
save_to_db = print

startPosition :: LogState
startPosition = LogState Outside []

parseLog :: FilePath -> IO ()
parseLog fname = do
    _ <- runStateT (exec fname) startPosition
    return ()
