module Main where

import System.Environment
import Data.Maybe
import ArgParse

main :: IO ()
main = do
  args <- getArgs
  case getArgsOptions ArgOptions { optUsage = Nothing, optFiles = [] } args of
      Left options -> if (isJust $ optUsage options) && (fromJust $ optUsage options) then
                        putStrLn argsUsage
                      else print options
      Right error -> putStrLn error
