{-# LANGUAGE OverloadedStrings #-}

module Bloop where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)

main = fileSum "main.hs" >>= print

fileSum path = fmap blobHash $ Lazy.readFile path

blobHash blob = toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length blob
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", blob]
