module Porcelain where

import Plumbing

import qualified Data.ByteString.Lazy.Char8 as Lazy

usage = "TODO: usage"

processArgs :: [String] -> IO ()
processArgs [] = putStrLn usage
processArgs (command:args)
  | command == "init" = initRepo
  | command == "mark" = markObjectsCommand args >>= putStr . unlines
  | command == "cat"  = catObjectCommand args >>= Lazy.putStr
  | otherwise         = putStrLn usage

-- TODO: this is probably duplicating work done in `addTree`
markObjectsCommand :: [FilePath] -> IO [BloopHash]
markObjectsCommand args = mapM Lazy.readFile args >>= mapM storeObject

catObjectCommand :: [BloopHash] -> IO Lazy.ByteString
catObjectCommand args = readObject objectHash
  where objectHash = head args
