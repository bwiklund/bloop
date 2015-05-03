{-# LANGUAGE OverloadedStrings #-}

module Test (main) where

import Test.HUnit
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Plumbing

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
      "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo.txt"
      (treeEntryToLine (Blob (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "foo.txt"))

  , assertEqual
      "tree -> tree entry serialization"
      "040000 tree 08cf6101416f0ce0dda3c80e627f333854c4085c fooDirectory"
      (treeEntryToLine (Tree (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "fooDirectory" []))

  , assertEqual
      "tree with multiple entry serialization"
      (unlines [
        "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo1.txt",
        "100755 blob 08cf6101416f0ce0dda3c80e627f333854c4085c foo2.txt",
        "040000 tree 08cf6101416f0ce0dda3c80e627f333854c4085c fooDirectory"
      ])
      (serializeObject (Tree (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "fooDirectory" [
        (Blob (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "foo1.txt"),
        (Blob (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "foo2.txt"),
        (Tree (Lazy.pack "08cf6101416f0ce0dda3c80e627f333854c4085c") "fooDirectory" [])
      ]))
  ]

main = do
  counts <- runTestTT $ tests
  if (errors counts + failures counts == 0) then exitSuccess else exitFailure
