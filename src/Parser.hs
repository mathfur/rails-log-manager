{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Peggy
import Type
import Data.List

[peggy|
nums :: [Int]
  = num*

num ::: Int
  = [0-9]+ { read $1 }

word :: String
  = [a-zA-Z0-9_-]+ { $1 }

ident ::: String
  = [a-zA-Z0-9_-]+ { $1 }

controller_name :: String
  = (word "::")* word { concat (intersperse "::" $1) ++ $2 }

ip ::: String
  = [0-9]+ "." [0-9]+ "." [0-9]+ "." [0-9]+ { $1 ++ "." ++ $2 ++ "." ++ $3 ++ "." ++ $4 }

date ::: String
  = [0-9]+ "-" [0-9]+ "-" [0-9]+ { $1 ++ "-" ++ $2 ++ "-" ++ $3 }

time ::: String
  = [0-9]+ ":" [0-9]+ ":" [0-9]+ { $1 ++ ":" ++ $2 ++ ":" ++ $3 }

-- pair :: (String, String)
-- pair = undefined
-- 
-- hash :: [(String, String)]
-- hash = '{' ((pair ',')* pair)* '}' { map (\a b -> a ++ [b]) $1 }

-------------------------------------------------

-- Processing Bar::FooController#export (for 127.0.0.1 at 2000-00-00 13:27:31) [GET]
start_line :: [(String, String)]
  = "Processing" controller_name '#' word "(for" ip "at" date time ') [' word ']'
    { [("controller", $1), ("action", $2), ("ip", $3), ("datetime", $4 ++ $5), ("method", $6)] }

-- Parameters: {"action"=>"export",  "cont..
parameters_line :: [(String, String)]
  = "Parameters:" [^\n]+ { [("parameters", $1)] }

-- Completed in 23ms (View: 1, DB: 3) | 403 Forbidden [http://localhost/foo/bar]
end_line :: [(String, String)]
  = "Completed in" [0-9]+ 'ms' "(View:" [0-9]+ ", DB:" [0-9]+ ')' "|" [0-9]+ ident '[' [^\]]+ ']'
    { [("time", $1), ("view_time", $2), ("db_time", $3), ("response", $4 ++ $5), ("url", $6)] }

other_line :: [(String, String)]
  = (.+) { [("other", $1)] }

log_line :: LogLine
  = start_line / parameters_line / end_line / other_line

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
