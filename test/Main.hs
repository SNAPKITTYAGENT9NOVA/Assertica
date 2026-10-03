module Main where

import Test.Tasty

import qualified Test.Core.AST as AST
import qualified Test.Core.Equality as Equality
import qualified Test.Core.Invariants as Invariants
import qualified Test.Core.ProofTerm as ProofTerm
import qualified Test.Core.TypeChecker as TypeChecker

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Assertica"
  [ AST.tests
  , Equality.tests
  , Invariants.tests
  , ProofTerm.tests
  , TypeChecker.tests
  ]
