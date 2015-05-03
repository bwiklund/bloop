module Porcelain where

import Plumbing

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as Lazy

main = getArgs >>= processArgs

usage = "TODO: usage"

processArgs [] = putStrLn usage
processArgs (command:args)
  | command == "init" = initRepo
  | command == "mark" = markObjectCommand args >>= putStrLn . unlines
  | otherwise         = putStrLn usage

markObjectCommand args = mapM Lazy.readFile args >>= (mapM storeObject)
