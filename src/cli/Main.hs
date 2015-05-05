module Main where

import Porcelain (processArgs)

import System.Environment

main :: IO ()
main = getArgs >>= processArgs
