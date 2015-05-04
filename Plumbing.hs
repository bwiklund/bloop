{-# LANGUAGE OverloadedStrings #-}

module Plumbing where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import qualified Codec.Compression.Zlib as Zlib
import System.Directory (createDirectory, createDirectoryIfMissing, getDirectoryContents)
import System.FilePath ((</>), dropFileName)


-- some paths in case we need to override these
bloopDirName = ".bloop"
bloopObjectsDirName = "objects"

bloopObjectsPath = bloopDirName </> bloopObjectsDirName

-- create a new repo (or fail if one already exists)
initRepo = mapM_ createDirectory
  [ bloopDirName
  , bloopObjectsPath
  ]

-- deserialized objects
-- TODO: separate object from object details (blob/tree/commit/tag)
data BloopTree
  = Tree {hash :: Lazy.ByteString, fileName :: FilePath, entries :: [BloopTree]}
  | Blob {hash :: Lazy.ByteString, fileName :: FilePath, fileContents :: Lazy.ByteString}
  deriving (Eq, Show)

-- read a file in and hash it... remove me because i belong elsewhere
fileSum path = fmap blobHash $ Lazy.readFile path

-- convert a bytestream to a hash
blobHash blob = toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length blob
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", blob]

-- where to store a hash
pathForHash hash = bloopObjectsPath </> prefix </> suffix
  where (prefix, suffix) = splitAt 2 hash

-- store a serialized object, returning its hash
-- storeObject :: Lazy.ByteString -> IO String
storeObject bs = do
  createDirectoryIfMissing True (dropFileName path)
  Lazy.writeFile path compressed
  return hash
  where path = pathForHash hash
        hash = Lazy.unpack $ blobHash bs
        compressed = compressSerialized bs

-- read an object given its hash
readObject hash = fmap decompressSerialized $ Lazy.readFile path
  where path = pathForHash hash

-- TODO: make recursive
-- add all blobs in a directory, then write a tree with their records
addTree path = do
  filePaths <- scanDirectory path
  fileContents <- mapM Lazy.readFile filePaths
  hashes <- mapM storeObject fileContents
  --TODO: don't fake the filenames and object types
  let fixmeTreeContents = Lazy.unlines $ map (\h -> Lazy.concat ["100644 blob ", Lazy.pack h, " ", Lazy.pack h, ".bar"]) hashes
  storeObject fixmeTreeContents

-- TODO: make recursive
instantiateTree hash = do
  treeRecordContents <- readObject hash
  let recordLines = Lazy.lines treeRecordContents
      blobs = map recordLineToBlob recordLines
  mapM createFileFromObject blobs
  where createFileFromObject (Blob hash fileName _) =
          readObject (Lazy.unpack hash) >>= Lazy.writeFile (fileName ++ "foo")
        recordLineToBlob str = Blob hash (Lazy.unpack fileName) ""
          where (perm:btype:hash:fileName:_) = Lazy.words str

-- scanDirectory :: FilePath -> BloopTree
scanDirectory path = do
  contents <- getDirectoryContents path
  return $ filterPaths contents
  where filterPaths = filter (\path -> path /= "." && path /= ".." && path /= ".bloop" && path /= ".git") -- TODO: .bloopignore

-- to make it easy to swap out compression algorithms
compressSerialized = Zlib.compress

-- to make it easy to swap out compression algorithms
decompressSerialized = Zlib.decompress

serializeObject :: BloopTree -> Lazy.ByteString
serializeObject (Tree {entries = treeEntries}) = Lazy.pack $ unlines $ map (Lazy.unpack . treeEntryToLine) treeEntries
serializeObject (Blob {fileContents = _fileContents}) = _fileContents

-- TODO: if we're matching git, the hashes are binary and the names are null terminated.
treeEntryToLine :: BloopTree -> Lazy.ByteString
treeEntryToLine (Blob {hash = entryHash, fileName = entryFileName}) = Lazy.concat ["100755 blob ", entryHash, " ", Lazy.pack entryFileName]
treeEntryToLine (Tree {hash = entryHash, fileName = entryFileName}) = Lazy.concat ["040000 tree ", entryHash, " ", Lazy.pack entryFileName]
-- treeEntryToLine _ = "undefined"
