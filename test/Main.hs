{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Plumbing

tests :: Test
tests = TestList $ map TestCase
  [ assertEqual
      "blob hash"
      "08cf6101416f0ce0dda3c80e627f333854c4085c"
      (blobHash "test content")

  , assertEqual
      "object path"
      ".bloop/objects/08/cf6101416f0ce0dda3c80e627f333854c4085c"
      (pathForHash "08cf6101416f0ce0dda3c80e627f333854c4085c")

  , assertEqual
      "object compression"
      "the quick brown fox etc"
      (decompressSerialized $ compressSerialized "the quick brown fox etc")

  , assertEqual
      "tree -> blob entry serialization"
      (Lazy.pack "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo.txt")
      (treeEntryToLine (Blob "08cf6101416f0ce0dda3c80e627f333854c4085c" "foo.txt" "test content"))

  , assertEqual
      "tree -> tree entry serialization"
      (Lazy.pack "040000 tree 08cf6101416f0ce0dda3c80e627f333854c4085c fooDirectory")
      (treeEntryToLine (Tree "08cf6101416f0ce0dda3c80e627f333854c4085c" "fooDirectory" []))

  , assertEqual
      "tree with multiple entry serialization"
      (Lazy.pack $ unlines [
        "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo1.txt",
        "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo2.txt",
        "040000 tree 08cf6101416f0ce0dda3c80e627f333854c4085c fooDirectory"
      ])
      (serializeObject (Tree "08cf6101416f0ce0dda3c80e627f333854c4085c" "fooDirectory" [
        Blob "08cf6101416f0ce0dda3c80e627f333854c4085c" "foo1.txt" "test content",
        Blob "08cf6101416f0ce0dda3c80e627f333854c4085c" "foo2.txt" "test content",
        Tree "08cf6101416f0ce0dda3c80e627f333854c4085c" "fooDirectory" []
      ]))
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result == 0 then exitSuccess else exitFailure
