{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Core.Invariants
Description : Tests for AST invariants
-}

module Test.Core.Invariants
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set
import Data.Text (Text)

import Assertica.Core.AST
import Assertica.Core.Invariants

tests :: TestTree
tests = testGroup "Assertica.Core.Invariants"
  [ testGroup "Binding Invariants"
      [ testCase "All variables bound" testAllVarsBound
      , testCase "Free variables detected" testFreeVarsDetected
      ]
  , testGroup "Qualification Invariants"
      [ testCase "Valid qualification" testValidQual
      , testCase "Invalid empty local name" testInvalidQual
      ]
  , testGroup "Type Well-Formedness"
      [ testCase "Well-formed simple type" testWellFormedType
      ]
  , testGroup "Term Well-Formedness"
      [ testCase "Well-formed term" testWellFormedTerm
      ]
  ]

-- ============================================================================
-- BINDING INVARIANT TESTS
-- ============================================================================

testAllVarsBound :: Assertion
testAllVarsBound =
  let x = Var "x" 1
      binder = Binder x Nothing
      body = Var x
      lam = Lam binder body
  in allVariablesBound lam @? "All variables should be bound in λx. x"

testFreeVarsDetected :: Assertion
testFreeVarsDetected =
  let x = Var "x" 1
      y = Var "y" 2
      binder = Binder x Nothing
      body = App (Var x) (Var y)
      lam = Lam binder body
  in not (allVariablesBound lam) @? "Free variable y should be detected"

-- ============================================================================
-- QUALIFICATION INVARIANT TESTS
-- ============================================================================

testValidQual :: Assertion
testValidQual =
  let q = QName ["Prelude"] "map"
  in case checkQualificationInvariant q of
       Right () -> pure ()
       Left _ -> assertFailure "Valid qualification failed"

testInvalidQual :: Assertion
testInvalidQual =
  let q = QName ["Prelude"] ""  -- Empty local name
  in case checkQualificationInvariant q of
       Left _ -> pure ()
       Right () -> assertFailure "Should have rejected empty local name"

-- ============================================================================
-- TYPE WELL-FORMEDNESS TESTS
-- ============================================================================

testWellFormedType :: Assertion
testWellFormedType =
  let ty = TyFun (TyVar (Var "a" 1)) (TyVar (Var "a" 1))
  in case isWellFormedType ty of
       Right () -> pure ()
       Left _ -> assertFailure "Type should be well-formed"

-- ============================================================================
-- TERM WELL-FORMEDNESS TESTS
-- ============================================================================

testWellFormedTerm :: Assertion
testWellFormedTerm =
  let x = Var "x" 1
      binder = Binder x Nothing
      term = Lam binder (Var x)
  in case isWellFormedTerm term of
       Right () -> pure ()
       Left _ -> assertFailure "Term should be well-formed"
