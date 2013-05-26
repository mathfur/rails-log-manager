{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -Wall #-}

module Parser where

import Text.Peggy
import Type

[peggy|
nums :: [Int]
  = num*

num ::: Int
  = [0-9]+ { read $1 }

word :: String
  = [a-zA-Z0-9_-]+ { $1 }

ident ::: String
  = [a-zA-Z0-9_-]+ { $1 }

-------------------------------------------------

-- Processing AccountController#login [POST]
start_line :: [(String, String)]
  = "Processing" word "#" word "[" word "]" { [("controller", $1), ("action", $2), ("method", $3)] }

-- Parameters: {"action"=>"login",  "authenticity_token"=>"[FILTERED]", "username"=>"[FILTERED]",  "controller"=>"account",  "password"=>"[FILTERED]", "login"=>"ログイン &#187;"}
parameters_line :: [(String, String)]
  = "Parameters:" [^\n]+ { [("params", $1)] }

-- Completed 200 OK in 1648ms (Views: 568.2ms | ActiveRecord: 3.2ms)
end_line :: [(String, String)]
  = "Completed" [0-9]+ ident "in" [0-9]+ 'ms' { [("code", $1), ("response_name", $2), ("time", $3)] }

other_line :: [(String, String)]
  = (.+) { [("other", $1)] }

log_line :: LogLine
  = start_line / parameters_line / other_line / end_line

|]

parseLogLine :: String -> [(String,String)]
parseLogLine s = case (parseString log_line "()" s) of
          Right x -> x
          Left _ -> []

inputKind :: String -> Input
inputKind s = case (parseString start_line "" s) of
                Right _ -> Start
                _ -> case (parseString end_line "" s) of
                       Right _ -> End
                       _       -> Other
