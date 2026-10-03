module Main where

import Test.Tasty

import qualified Test.Core.AST as AST
import qualified Test.Core.Equality as Equality
import qualified Test.Core.Invariants as Invariants
import qualified Test.Core.Module as Module
import qualified Test.Core.ProofTerm as ProofTerm
import qualified Test.Core.TypeChecker as TypeChecker
import qualified Test.Core.Termination as Termination
import qualified Test.Core.Positivity as Positivity
import qualified Test.Core.Pattern as Pattern
import qualified Test.Surface.Lexer as SurfaceLexer
import qualified Test.Surface.Parser as SurfaceParser
import qualified Test.Surface.Elaborator as SurfaceElaborator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Assertica"
  [ AST.tests
  , Equality.tests
  , Invariants.tests
  , Module.tests
  , ProofTerm.tests
  , TypeChecker.tests
  , Termination.tests
  , Positivity.tests
  , Pattern.tests
  , SurfaceLexer.tests
  , SurfaceParser.tests
  , SurfaceElaborator.tests
  ]
