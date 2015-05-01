{-# LANGUAGE OverloadedStrings #-}

module Bloop where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy

main = fileSum "main.hs" >>= putStrLn . show

fileSum path = do
  contents <- Lazy.readFile path
  let len = Lazy.length contents
  return $ hashlazy $ Lazy.concat ["blob ", Lazy.pack $ show len, "\0", contents]

