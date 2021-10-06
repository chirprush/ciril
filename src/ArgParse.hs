module ArgParse where

import Data.List
import Text.Printf

data ArgOptions = ArgOptions {
      optUsage :: Maybe Bool
    , optFiles :: [String]
    } deriving (Show)

data FlagType = FlagUsage | FlagInvalid

type ArgError = String

isArgFlag :: String -> Maybe String
isArgFlag flag
  | flag == "--" || flag == "-" = Nothing
  | isPrefixOf "--" flag && length flag > 2 = Just (drop 2 flag)
  | isPrefixOf "-" flag && length flag > 1 = Just (drop 1 flag)
  | otherwise = Nothing

getFlagType :: String -> FlagType
getFlagType flag = case flag of
                     "h" -> FlagUsage
                     "help" -> FlagUsage
                     _ -> FlagInvalid
  
getFlagArity :: FlagType -> Int
getFlagArity flag = case flag of
                      FlagUsage -> 0
                      FlagInvalid -> 0

getArgsOptions :: ArgOptions -> [String] -> Either ArgOptions ArgError
getArgsOptions options [] = Left options
getArgsOptions options (first:rest) = case isFlag of
  Just flag -> case flagType of
                 FlagUsage -> getArgsOptions options{optUsage = Just True} rest'
                 FlagInvalid -> Right $ printf "Invalid flag name `%s`" flag
               where flagType = getFlagType flag 
                     arity = getFlagArity flagType
                     (params, rest') = splitAt arity rest
  Nothing -> case first of
    "--" -> getArgsOptions options rest
    "-" -> getArgsOptions options rest
    _ -> getArgsOptions options{optFiles = first : (optFiles options)} rest
  where isFlag = isArgFlag first

argsUsage :: String
argsUsage =    "Usage: ciril [OPTIONS] FILES\n"
            ++ "-h, --help: Shows this usage page"
