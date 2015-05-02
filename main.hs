{-# LANGUAGE OverloadedStrings #-}

module Bloop where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)

-- main = fileSum "main.hs" >>= print
-- main = storeObject "test content"
main = print $ (blobHash "test content") == "08cf6101416f0ce0dda3c80e627f333854c4085c"

fileSum path = fmap blobHash $ Lazy.readFile path

blobHash blob = toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length blob
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", blob]

-- take some data and store it in the filesystem by hash
storeObject bs = Lazy.writeFile path bs
  where path = Lazy.unpack $ blobHash bs
