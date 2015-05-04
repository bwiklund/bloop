{-# LANGUAGE OverloadedStrings #-}

module Plumbing where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Char8 as Strict
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
initRepo :: IO ()
initRepo = mapM_ createDirectory
  [ bloopDirName
  , bloopObjectsPath
  ]

type BloopHash = String

-- deserialized objects
-- TODO: separate object from object details (blob/tree/commit/tag)
-- TODO: break into two data types, objects (the actual data) and pointers (the records in tree objects)?
data BloopTree
  = Tree {hash :: BloopHash, fileName :: FilePath, entries :: [BloopTree]}
  | Blob {hash :: BloopHash, fileName :: FilePath, fileContents :: Lazy.ByteString}
  deriving (Eq, Show)

createTree :: FilePath -> [BloopTree] -> BloopTree
createTree fileName entries = Tree h fileName entries
  where h = blobHash $ serializeTreeEntries entries

-- convert a bytestream to a hash
blobHash :: Lazy.ByteString -> BloopHash
blobHash bs = Lazy.unpack $ toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length bs
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", bs]

-- where to store a hash
pathForHash :: String -> FilePath
pathForHash hash = bloopObjectsPath </> prefix </> suffix
  where (prefix, suffix) = splitAt 2 hash

-- store a serialized object, returning its hash
-- storeObject :: Lazy.ByteString -> IO String
storeObject :: Lazy.ByteString -> IO String
storeObject bs = do
  createDirectoryIfMissing True (dropFileName path)
  Lazy.writeFile path compressed
  return hash
  where path = pathForHash hash
        hash = blobHash bs
        compressed = compressSerialized bs

-- read an object given its hash
readObject :: BloopHash -> IO Lazy.ByteString
readObject hash = fmap decompressSerialized $ Lazy.readFile path
  where path = pathForHash hash

-- TODO: make recursive
-- add all blobs in a directory, then write a tree with their records
addTree :: FilePath -> IO BloopHash
addTree path = do
  filePaths <- scanDirectory path
  fileContents <- mapM Lazy.readFile filePaths
  hashes <- mapM storeObject fileContents
  --TODO: don't fake the object types
  -- Lazy.concat ["100644 blob ", Lazy.pack h, " ", Lazy.pack fn]
  let fixmeTreeEntries = zipWith (\h fn -> Blob h fn "") hashes filePaths
      newTree = createTree "." fixmeTreeEntries
  storeObject $ serializeObject newTree

-- TODO: make recursive and dry up
instantiateTree :: String -> FilePath -> IO ()
instantiateTree hash path = do
  treeRecordContents <- readObject hash
  let blobs = map treeLineToNode $ Lazy.lines treeRecordContents
  createDirectoryIfMissing True path
  mapM_ (instantiateBlob path) blobs

instantiateBlob ::  FilePath -> BloopTree -> IO ()
instantiateBlob path (Blob hash fileName _) =
  readObject hash >>= Lazy.writeFile (path </> fileName)

scanDirectory :: FilePath -> IO [FilePath]
scanDirectory path = do
  contents <- getDirectoryContents path
  return $ filterPaths contents
  where filterPaths = filter (\path -> path /= "." && path /= ".." && path /= ".bloop" && path /= ".git") -- TODO: .bloopignore

-- to make it easy to swap out compression algorithms
compressSerialized :: Lazy.ByteString -> Lazy.ByteString
compressSerialized = Zlib.compress

-- to make it easy to swap out compression algorithms
decompressSerialized :: Lazy.ByteString -> Lazy.ByteString
decompressSerialized = Zlib.decompress

serializeObject :: BloopTree -> Lazy.ByteString
serializeObject (Tree {entries = treeEntries}) = serializeTreeEntries treeEntries
serializeObject (Blob {fileContents = _fileContents}) = _fileContents

serializeTreeEntries :: [BloopTree] -> Lazy.ByteString
serializeTreeEntries treeEntries = Lazy.pack $ unlines $ map (Lazy.unpack . treeEntryToLine) treeEntries

-- TODO: if we're matching git, the hashes are binary and the names are null terminated.
treeEntryToLine :: BloopTree -> Lazy.ByteString
treeEntryToLine (Blob {hash = entryHash, fileName = entryFileName}) = Lazy.concat ["100755 blob ", Lazy.pack entryHash, " ", Lazy.pack entryFileName]
treeEntryToLine (Tree {hash = entryHash, fileName = entryFileName}) = Lazy.concat ["040000 tree ", Lazy.pack entryHash, " ", Lazy.pack entryFileName]
-- treeEntryToLine _ = "undefined"

treeLineToNode :: Lazy.ByteString -> BloopTree
treeLineToNode str = Blob (Lazy.unpack hash) (Lazy.unpack fileName) ""
  where (perm:btype:hash:fileName:_) = Lazy.words str
