module Main where

import Test.Tasty

import qualified Test.Core.AST as AST
import qualified Test.Core.Equality as Equality
import qualified Test.Core.Invariants as Invariants
import qualified Test.Core.ProofTerm as ProofTerm

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Assertica"
  [ AST.tests
  , Equality.tests
  , Invariants.tests
  , ProofTerm.tests
  ]
