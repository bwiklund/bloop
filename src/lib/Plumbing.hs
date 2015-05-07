{-# LANGUAGE OverloadedStrings #-}

-- throughout this project, I try name IO functions in a way to describes what kind of IO they do.
-- read: only reads.
-- write: only writes.
-- readAndStore: self explanatory. these should (ideally) be short functions that compose read/write counterparts.

module Plumbing where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.ByteString.Lazy.Builder (toLazyByteString, byteStringHex)
import qualified Codec.Compression.Zlib as Zlib
import System.Directory (createDirectory, createDirectoryIfMissing, getDirectoryContents)
import System.FilePath ((</>), dropFileName, takeFileName)
import System.Posix.Files (isDirectory, getFileStatus)
import Control.Applicative ((<$>))



type BloopHash = String

-- deserialized objects
-- TODO: separate object from object details (blob/tree/commit/tag)
-- TODO: break into two data types, objects (the actual data) and pointers (the records in tree objects)?
data BloopTree
  = Tree {hash :: BloopHash, fileName :: FilePath, entries :: [BloopTree]}
  | Blob {hash :: BloopHash, fileName :: FilePath, fileContents :: Lazy.ByteString}
  deriving (Eq, Show)

buildTree :: FilePath -> [BloopTree] -> BloopTree
buildTree treePath treeEntries = Tree h treePath treeEntries
  where h = blobHash $ serializeTreeEntries treeEntries

-- some paths in case we need to override these
bloopDirName = ".bloop"
bloopObjectsDirName = "objects"
bloopObjectsPath = bloopDirName </> bloopObjectsDirName


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
treeEntryToLine blob@Blob{} = Lazy.pack $ concat ["100755 blob ", hash blob, " ", fileName blob]
treeEntryToLine tree@Tree{} = Lazy.pack $ concat ["040000 tree ", hash tree, " ", fileName tree]
-- treeEntryToLine _ = "undefined"

-- TODO: record types instead of full object types
-- TODO: perms for files
treeLineToRecord :: Lazy.ByteString -> BloopTree
treeLineToRecord str =
  case bType of
    "blob" -> Blob bHash bFileName ""
    "tree" -> Tree bHash bFileName []
    _      -> error "not a valid object type"
  where (_:bType:bHash:bFileName:_) = words $ Lazy.unpack str



-- Everything IO related.



-- create a new repo (or fail if one already exists)
initRepo :: IO ()
initRepo = mapM_ createDirectory
  [ bloopDirName
  , bloopObjectsPath
  ]

-- convert a bytestream to a hash
blobHash :: Lazy.ByteString -> BloopHash
blobHash bs = Lazy.unpack $ toHexHash headerAndContents
  where toHexHash = toLazyByteString . byteStringHex . hashlazy
        len = Lazy.length bs
        headerAndContents = Lazy.concat ["blob ", Lazy.pack $ show len, "\0", bs]

-- where to store a hash
pathForHash :: BloopHash -> FilePath
pathForHash h = bloopObjectsPath </> prefix </> suffix
  where (prefix, suffix) = splitAt 2 h

-- store a serialized object, returning its hash
storeObject :: Lazy.ByteString -> IO BloopHash
storeObject bs = do
  createDirectoryIfMissing True (dropFileName path)
  Lazy.writeFile path compressed
  return objHash
  where path = pathForHash objHash
        objHash = blobHash bs
        compressed = compressSerialized bs

-- read an object given its hash
readObject :: BloopHash -> IO Lazy.ByteString
readObject bHash = decompressSerialized <$> Lazy.readFile path
  where path = pathForHash bHash

-- add all blobs in a directory, then write a tree with their records
-- TODO: store recursively. currently directories are not saved, only read in.
readAndStoreTreeRecursive :: FilePath -> IO BloopHash
readAndStoreTreeRecursive fPath = readTreeRecursive fPath >>= storeTreeRecursive

readTreeRecursive :: FilePath -> IO BloopTree
readTreeRecursive fPath = do
  filePaths <- scanDirectory fPath
  objects <- mapM (\fName -> readFileToObject $ fPath </> fName) filePaths
  return $ buildTree (takeFileName fPath) objects

-- store a tree recursively, depth first. that way, if it's interrupted, the
-- top tree won't be written yet, so will be atomic (other than filling the
-- object db with unreferenced objects)
storeTreeRecursive :: BloopTree -> IO BloopHash
storeTreeRecursive tree = do
  mapM_ storeEntryRecursive (entries tree)
  serializeAndStore tree
  where storeEntryRecursive t@Tree{} = storeTreeRecursive t
        storeEntryRecursive b@Blob{} = serializeAndStore b
        serializeAndStore = storeObject . serializeObject

readFileToObject :: FilePath -> IO BloopTree
readFileToObject fPath = do
  isDir <- isDirectory <$> getFileStatus fPath
  if isDir
    then readTreeRecursive fPath
    else do -- TODO: make this a one liner
      bs <- Lazy.readFile fPath
      let bHash = blobHash bs
      return $ Blob bHash (takeFileName fPath) bs

instantiateTreeRecursive :: BloopHash -> FilePath -> IO ()
instantiateTreeRecursive tHash tPath = do
  createDirectoryIfMissing True tPath
  readObject tHash >>= mapM_ ((\entry -> instantiateTreeEntryRecursive entry tPath) . treeLineToRecord) . Lazy.lines

-- reads the object from the database and instantiates it.
-- TODO: this probably belongs scoped inside the above fn. like definitely.
-- TODO: BloopTree should be BloopRecord. i'm passing in partial objects (blobs with no content) which is janky.
instantiateTreeEntryRecursive :: BloopTree -> FilePath -> IO ()
instantiateTreeEntryRecursive o@(Blob bHash bFileName _) relativePath = do
  putStrLn $ show o
  readObject bHash >>= Lazy.writeFile (relativePath </> bFileName)
instantiateTreeEntryRecursive o@(Tree tHash tFileName _) relativePath = do
  putStrLn $ show o
  instantiateTreeRecursive tHash (relativePath </> tFileName)

-- return a list of files in a directory, excluding stuff that should be ignored
scanDirectory :: FilePath -> IO [FilePath]
scanDirectory path = do
  contents <- getDirectoryContents path
  return $ filterPaths contents
  where filterPaths = filter (\p -> p /= "." && p /= ".." && p /= ".bloop" && p /= ".git") -- TODO: .bloopignore
