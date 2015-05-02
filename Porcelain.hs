module Porcelain where

import Plumbing

import System.Environment

main = getArgs >>= processArgs

doUsage = putStrLn "TODO: usage"

processArgs [] = doUsage
processArgs (command:args)
  | command == "init" = initRepo
  | otherwise         = doUsage
