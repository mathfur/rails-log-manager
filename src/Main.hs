{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Manager

main :: IO ()
main = parseLog "log/development.log"
