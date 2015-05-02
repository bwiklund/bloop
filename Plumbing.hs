{-# LANGUAGE OverloadedStrings #-}

module Plumbing where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import qualified Codec.Compression.Zlib as Zlib
import System.Directory (createDirectory)
import System.FilePath.Posix ((</>))

-- main = fileSum "main.hs" >>= print
-- main = storeObject "test content"

bloopDir = ".bloop"
bloopObjectsDirName = "objects"
bloopObjectsFullPath = bloopDir </> bloopObjectsDirName

initRepo = mapM_ createDirectory [ bloopDir
                                 , bloopObjectsFullPath
                                 ]

fileSum path = fmap blobHash $ Lazy.readFile path

blobHash blob = toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length blob
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", blob]

-- where to store a hash
pathForHash hash = bloopObjectsFullPath </> prefix </> suffix
  where (prefix, suffix) = splitAt 2 hash

-- store an object, returning its hash
storeObject bs = do
  Lazy.writeFile path compressed
  return hash
  where path = pathForHash hash
        hash = Lazy.unpack $ blobHash bs
        compressed = Zlib.compress bs

-- read an object given its hash
readObject hash = fmap Zlib.decompress $ Lazy.readFile path
  where path = pathForHash hash
