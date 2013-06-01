{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Type where

type RailsLog = [(String, String)]
type LogLine = [(String, String)]
data Input = Start | End | Other deriving (Show, Eq)
data Side = Outside | Inside deriving (Show, Eq)
