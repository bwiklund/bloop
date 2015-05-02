module Test (main) where

import Test.HUnit
import System.Exit

import Plumbing

tests = TestList $ map TestCase [

    assertEqual "foo" 1 1

  ]

main = do
  counts <- runTestTT $ tests
  if (errors counts + failures counts == 0) then exitSuccess else exitFailure
