{-|
Main test runner for Assertica
-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Test.Core.AST as AST
import qualified Test.Core.Assertion as Assertion
import qualified Test.Core.Equality as Equality
import qualified Test.Core.Invariants as Invariants

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Assertica Test Suite"
  [ AST.tests
  , Assertion.tests
  , Equality.tests
  , Invariants.tests
  ]
