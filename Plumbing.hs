{-# LANGUAGE OverloadedStrings #-}

module Plumbing where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import qualified Codec.Compression.Zlib as Zlib
import System.Directory (createDirectory)
import System.FilePath.Posix ((</>))


-- some paths in case we need to override these
bloopDir = ".bloop"
bloopObjectsDirName = "objects"
bloopObjectsFullPath = bloopDir </> bloopObjectsDirName

-- create a new repo (or fail if one already exists)
initRepo = mapM_ createDirectory
  [ bloopDir
  , bloopObjectsFullPath
  ]

-- read a file in and hash it... remove me because i belong elsewhere
fileSum path = fmap blobHash $ Lazy.readFile path

-- convert a bytestream to a hash
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
