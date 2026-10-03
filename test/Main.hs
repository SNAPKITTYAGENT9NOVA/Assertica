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
import qualified Test.StdLib.Setoid as StdLibSetoid
import qualified Test.StdLib.Lattice as StdLibLattice
import qualified Test.StdLib.Absorption as StdLibAbsorption
import qualified Test.StdLib.Monomorphism as StdLibMonomorphism
import qualified Test.Backend.CodeGen as BackendCodeGen
import qualified Test.Backend.TermCompiler as BackendTermCompiler
import qualified Test.Backend.TypeCompiler as BackendTypeCompiler
import qualified Test.Backend.ProofCompiler as BackendProofCompiler
import qualified Test.Backend.Emit as BackendEmit
import qualified Test.Backend.Integration as BackendIntegration
import qualified Test.Verify.PipelineTests as VerifyPipeline
import qualified Test.Verify.ReportTests as VerifyReport
import qualified Test.Verify.CITests as VerifyCI
import qualified Test.Verify.ErrorReportTests as VerifyErrorReport

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
  , StdLibSetoid.tests
  , StdLibLattice.tests
  , StdLibAbsorption.tests
  , StdLibMonomorphism.tests
  , BackendCodeGen.tests
  , BackendTermCompiler.tests
  , BackendTypeCompiler.tests
  , BackendProofCompiler.tests
  , BackendEmit.tests
  , BackendIntegration.tests
  , VerifyPipeline.tests
  , VerifyReport.tests
  , VerifyCI.tests
  , VerifyErrorReport.tests
  ]
