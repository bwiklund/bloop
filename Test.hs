{-# LANGUAGE OverloadedStrings #-}

module Test (main) where

import Test.HUnit
import System.Exit

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
      (decompressObject $ compressObject "the quick brown fox etc")
  ]

main = do
  counts <- runTestTT $ tests
  if (errors counts + failures counts == 0) then exitSuccess else exitFailure
