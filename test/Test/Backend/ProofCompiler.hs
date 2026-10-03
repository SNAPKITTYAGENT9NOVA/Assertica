{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.ProofCompiler
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Backend.CodeGen (defaultConfig)
import Assertica.Backend.ProofCompiler

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.ProofCompiler"
  [ testGroup "Basic Proofs"
      [ testCase "Reflexivity proof" testReflexivity
      , testCase "Symmetry proof" testSymmetry
      , testCase "Transitivity proof" testTransitivity
      ]
  , testGroup "Proof Variables"
      [ testCase "Proof variable" testProofVariable
      ]
  , testGroup "Proof Constructors"
      [ testCase "Simple proof constructor" testSimpleProofConstructor
      , testCase "Proof constructor with args" testProofConstructorWithArgs
      ]
  , testGroup "Introduction and Elimination"
      [ testCase "Proof introduction" testProofIntroduction
      , testCase "Proof elimination" testProofElimination
      ]
  , testGroup "Proof Application"
      [ testCase "Application of proof to term" testProofApplication
      ]
  , testGroup "Opaque Proofs"
      [ testCase "Opaque proof" testOpaqueProof
      ]
  , testGroup "Proof Annotation"
      [ testCase "Proof annotation" testProofAnnotation
      ]
  , testGroup "Determinism"
      [ testCase "Proof compilation is deterministic" testProofDeterminism
      ]
  ]

-- ============================================================================
-- UNIT TESTS: BASIC PROOFS
-- ============================================================================

testReflexivity :: Assertion
testReflexivity = do
  let x = Var (Var "x" 0)
  let refl = Refl x
  result <- case compileProof defaultConfig refl of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be Refl" "Refl" result

testSymmetry :: Assertion
testSymmetry = do
  let x = Var (Var "x" 0)
  let baseProof = Refl x
  let symProof = Symm baseProof
  result <- case compileProof defaultConfig symProof of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Symm" ("Symm" `T.isInfixOf` result)
  assertBool "Should contain Refl" ("Refl" `T.isInfixOf` result)

testTransitivity :: Assertion
testTransitivity = do
  let x = Var (Var "x" 0)
  let proof1 = Refl x
  let proof2 = Refl x
  let transProof = Trans proof1 proof2
  result <- case compileProof defaultConfig transProof of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Trans" ("Trans" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: PROOF VARIABLES
-- ============================================================================

testProofVariable :: Assertion
testProofVariable = do
  let pv = ProofVar (Var "p" 0)
  result <- case compileProof defaultConfig pv of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be p" "p" result

-- ============================================================================
-- UNIT TESTS: PROOF CONSTRUCTORS
-- ============================================================================

testSimpleProofConstructor :: Assertion
testSimpleProofConstructor = do
  let ctor = Constr' (QName [] "Constructor") []
  result <- case compileProof defaultConfig ctor of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be Constructor" "Constructor" result

testProofConstructorWithArgs :: Assertion
testProofConstructorWithArgs = do
  let x = Var (Var "x" 0)
  let proof1 = Refl x
  let proof2 = Refl x
  let ctor = Constr' (QName ["Data"] "Pair") [proof1, proof2]
  result <- case compileProof defaultConfig ctor of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Pair" ("Pair" `T.isInfixOf` result)
  assertBool "Should contain Refl" ("Refl" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: INTRO/ELIM
-- ============================================================================

testProofIntroduction :: Assertion
testProofIntroduction = do
  let x = Var (Var "x" 0)
  let baseProof = Refl x
  let intro = Intro (Binder (Var "h" 0) Nothing) baseProof
  result <- case compileProof defaultConfig intro of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain lambda" ("\\" `T.isInfixOf` result)
  assertBool "Should contain h" ("h" `T.isInfixOf` result)

testProofElimination :: Assertion
testProofElimination = do
  let x = Var (Var "x" 0)
  let proof1 = Refl x
  let proof2 = Refl x
  let elim = Elim proof1 proof2
  result <- case compileProof defaultConfig elim of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  -- Should contain two applications of Refl
  assertBool "Should be valid" (not (T.null result))

-- ============================================================================
-- UNIT TESTS: PROOF APPLICATION
-- ============================================================================

testProofApplication :: Assertion
testProofApplication = do
  let p = ProofVar (Var "p" 0)
  let x = Var (Var "x" 0)
  let appProof = App' p x
  result <- case compileProof defaultConfig appProof of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain p" ("p" `T.isInfixOf` result)
  assertBool "Should contain x" ("x" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: OPAQUE PROOFS
-- ============================================================================

testOpaqueProof :: Assertion
testOpaqueProof = do
  let prop = Eq (Var (Var "x" 0)) (Var (Var "y" 0))
  let opaque = Opaque (QName ["Axioms"] "axiom1") prop
  result <- case compileProof defaultConfig opaque of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should reference proof" ("proof" `T.isInfixOf` result)
  assertBool "Should contain axiom1" ("axiom1" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: PROOF ANNOTATION
-- ============================================================================

testProofAnnotation :: Assertion
testProofAnnotation = do
  let x = Var (Var "x" 0)
  let proof = Refl x
  let prop = Eq x x
  let annProof = ProofAnn proof prop
  result <- case compileProof defaultConfig annProof of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Refl" ("Refl" `T.isInfixOf` result)

-- ============================================================================
-- PROPERTY TESTS: DETERMINISM
-- ============================================================================

testProofDeterminism :: Assertion
testProofDeterminism = do
  let x = Var (Var "x" 0)
  let proof = Symm (Refl x)
  let result1 = compileProof defaultConfig proof
  let result2 = compileProof defaultConfig proof
  assertEqual "Proof compilation should be deterministic" result1 result2
