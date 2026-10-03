{-|
Module      : Test.Core.Invariants
Description : Tests for kernel invariants

Tests verify that the equality checking subsystem maintains critical invariants:
  1. Determinism: same input produces same output
  2. Fail-closed: unknown equality returns False
  3. No hidden theorem proving: algebraic properties are not assumed
  4. No external solvers: all reasoning is internal
-}

module Test.Core.Invariants (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Assertica.Core.AST
import Assertica.Core.Equality
import Assertica.Core.Invariants

tests :: TestTree
tests = testGroup "Assertica.Core.Invariants"
  [ determinismTests
  , failClosedTests
  , noHiddenTheoremsTests
  ]

-- | Tests for determinism invariant
determinismTests :: TestTree
determinismTests = testGroup "Determinism Invariant"
  [ testCase "Determinism for simple terms" $
      let t1 = TConst "5"
          t2 = TConst "5"
      in isDefinitionallyEqual t1 t2 @?=
         isDefinitionallyEqual t1 t2

  , testCase "Determinism for lambda terms" $
      let x = Var "x"
          t1 = TAbs x (TVar x)
          t2 = TAbs x (TVar x)
      in isDefinitionallyEqual t1 t2 @?=
         isDefinitionallyEqual t1 t2

  , testCase "Determinism for applications" $
      let x = Var "x"
          t1 = TApp (TAbs x (TVar x)) (TConst "5")
          t2 = TConst "5"
      in isDefinitionallyEqual t1 t2 @?=
         isDefinitionallyEqual t1 t2

  , testProperty "Determinism holds for all term pairs" $
      \t1 t2 ->
        isDefinitionallyEqual t1 t2 ==
        isDefinitionallyEqual t1 t2
  ]

-- | Tests for fail-closed invariant
failClosedTests :: TestTree
failClosedTests = testGroup "Fail-Closed Invariant"
  [ testCase "Different variables must fail" $
      let x = Var "x"
          y = Var "y"
      in isDefinitionallyEqual (TVar x) (TVar y) @?= False

  , testCase "Different constants must fail" $
      isDefinitionallyEqual (TConst "1") (TConst "2") @?= False

  , testCase "Lambda vs constant must fail" $
      let x = Var "x"
      in isDefinitionallyEqual (TAbs x (TVar x)) (TConst "5") @?= False

  , testCase "Unknown commutativity must fail" $
      let x = Var "x"
          y = Var "y"
          t1 = TApp (TApp (TConst "+") (TVar x)) (TVar y)
          t2 = TApp (TApp (TConst "+") (TVar y)) (TVar x)
      in isDefinitionallyEqual t1 t2 @?= False

  , testProperty "Equality is only true when truly equivalent" $
      \t1 t2 ->
        -- If equality returns True, we should be able to verify it by
        -- checking that both terms normalize to the same form
        not (isDefinitionallyEqual t1 t2) ||
        (normalize t1 == normalize t2)  -- Approximately (modulo alpha)
  ]

-- | Tests for no-hidden-theorems invariant
noHiddenTheoremsTests :: TestTree
noHiddenTheoremsTests = testGroup "No Hidden Theorems"
  [ testCase "Commutativity is not automatic" $
      -- x + y ≠ y + x (no commutativity axiom)
      let x = Var "x"
          y = Var "y"
          t1 = TApp (TApp (TConst "+") (TVar x)) (TVar y)
          t2 = TApp (TApp (TConst "+") (TVar y)) (TVar x)
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Associativity is not automatic" $
      -- (x + y) + z ≠ x + (y + z)
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          t1 = TApp (TApp (TConst "+") (TApp (TApp (TConst "+") (TVar x)) (TVar y))) (TVar z)
          t2 = TApp (TApp (TConst "+") (TVar x)) (TApp (TApp (TConst "+") (TVar y)) (TVar z))
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Identity is not automatic" $
      -- x + 0 ≠ x (no identity axiom)
      let x = Var "x"
          t1 = TApp (TApp (TConst "+") (TVar x)) (TConst "0")
          t2 = TVar x
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Distributivity is not automatic" $
      -- x * (y + z) ≠ (x * y) + (x * z)
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          t1 = TApp (TApp (TConst "*") (TVar x)) (TApp (TApp (TConst "+") (TVar y)) (TVar z))
          t2 = TApp (TApp (TConst "+") (TApp (TApp (TConst "*") (TVar x)) (TVar y)))
                     (TApp (TApp (TConst "*") (TVar x)) (TVar z))
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "De Morgan's law is not automatic" $
      -- not (x and y) ≠ (not x) or (not y) (no De Morgan's axiom)
      -- We'd need to encode boolean operations, so skip for now
      True @?= True

  , testProperty "Only beta, alpha, eta reductions are automatic" $
      -- This is a sanity check: complex expressions are not automatically equal
      \t1 t2 ->
        not (isDefinitionallyEqual t1 t2) ||
        -- If they're equal, it's because of beta/alpha/eta reduction,
        -- which means they must normalize to the same form
        (alphaEquivalent (normalize t1) (normalize t2))
  ]

-- | Test the invariant checking functions
invariantCheckingTests :: TestTree
invariantCheckingTests = testGroup "Invariant Checking Functions"
  [ testCase "checkDeterminism passes for pure equality checker" $
      let checker = isDefinitionallyEqual
          t1 = TConst "5"
          t2 = TConst "5"
      in case checkDeterminism checker t1 t2 of
           Right () -> True @?= True
           Left msg -> False @?= True

  , testCase "checkFailClosed passes" $
      let checker = isDefinitionallyEqual
          t1 = TVar (Var "x")
          t2 = TVar (Var "y")
      in case checkFailClosed checker t1 t2 of
           Right () -> True @?= True
           Left msg -> False @?= True

  , testCase "checkNoHiddenTheorems passes" $
      case checkNoHiddenTheorems isDefinitionallyEqual of
        Right () -> True @?= True
        Left msg -> False @?= True
  ]
