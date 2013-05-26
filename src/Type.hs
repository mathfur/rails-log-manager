{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Type where

import Control.Monad.State

type RailsLog = [(String, String)]
type LogLine = [(String, String)]
data Input = Start | End | Other deriving (Show, Eq)
data LogState = LogState { loc :: LogLocation, stack :: [String] } deriving (Show, Eq)
data LogLocation = Outside | Inside deriving (Show, Eq)

type RailsLogManager = StateT LogState IO ()
