{-# LANGUAGE OverloadedStrings #-}

module Bloop where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import qualified System.Posix as Posix

main = fileSum "main.hs" >>= print

fileSum path = do
  contents <- Lazy.readFile path
  stat <- Posix.getFileStatus path
  let len = Posix.fileSize stat
      headerAndContents = ["blob ", Lazy.pack $ show len, "\0", contents]
  return $ toLazyByteString $ byteStringHex $ hashlazy $ Lazy.concat headerAndContents
