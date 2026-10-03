{-|
Module      : Test.Core.Equality
Description : Comprehensive tests for equality checking and reduction

Tests cover:
  1. Beta reduction
  2. Alpha equivalence
  3. Eta reduction
  4. Normalization
  5. Definitional equality
  6. NEGATIVE TESTS: Properties that should NOT be considered equal
  7. Property-based tests for invariants
-}

{-# LANGUAGE DeriveGeneric #-}

module Test.Core.Equality (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Assertica.Core.AST
import Assertica.Core.Equality

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

tests :: TestTree
tests = testGroup "Assertica.Core.Equality"
  [ betaReductionTests
  , alphaEquivalenceTests
  , etaReductionTests
  , normalizationTests
  , definitionalEqualityTests
  , negativeEqualityTests
  , propertyTests
  ]

-- | Tests for beta reduction
betaReductionTests :: TestTree
betaReductionTests = testGroup "Beta Reduction"
  [ testCase "Simple lambda application" $
      let x = Var "x"
          -- (λx. x) 5
          term = TApp (TAbs x (TVar x)) (TConst "5")
      in betaReduce term @?= TConst "5"

  , testCase "Lambda with body containing the variable" $
      let x = Var "x"
          -- (λx. x + 1) 5 -> 5 + 1 (we represent + as an application)
          -- For simplicity, we'll use a simpler test
          term = TApp (TAbs x (TVar x)) (TConst "42")
      in betaReduce term @?= TConst "42"

  , testCase "No reduction for non-application" $
      let x = Var "x"
          term = TAbs x (TVar x)
      in betaReduce term @?= term

  , testCase "No reduction when not a lambda at head" $
      let x = Var "x"
          term = TApp (TVar x) (TConst "5")
      in betaReduce term @?= term

  , testCase "Chained beta reduction" $
      let x = Var "x"
          y = Var "y"
          -- ((λx. λy. y) 5) 42 -> (λy. y) 42 -> 42
          term = TApp (TApp (TAbs x (TAbs y (TVar y))) (TConst "5")) (TConst "42")
      in betaReduce term @?= TConst "42"

  , testCase "Pair projection (fst)" $
      let a = TConst "10"
          b = TConst "20"
      in betaReduce (TFst (TPair a b)) @?= a

  , testCase "Pair projection (snd)" $
      let a = TConst "10"
          b = TConst "20"
      in betaReduce (TSnd (TPair a b)) @?= b
  ]

-- | Tests for alpha equivalence
alphaEquivalenceTests :: TestTree
alphaEquivalenceTests = testGroup "Alpha Equivalence"
  [ testCase "Same variable names are equivalent" $
      let x = Var "x"
      in alphaEquivalent (TVar x) (TVar x) @?= True

  , testCase "Different variable names are not equivalent" $
      let x = Var "x"
          y = Var "y"
      in alphaEquivalent (TVar x) (TVar y) @?= False

  , testCase "Simple lambda renaming" $
      let x = Var "x"
          y = Var "y"
          -- (λx. x) ≈ (λy. y)
          t1 = TAbs x (TVar x)
          t2 = TAbs y (TVar y)
      in alphaEquivalent t1 t2 @?= True

  , testCase "Nested lambda renaming" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          w = Var "w"
          -- (λx. λy. x) ≈ (λz. λw. z)
          t1 = TAbs x (TAbs y (TVar x))
          t2 = TAbs z (TAbs w (TVar z))
      in alphaEquivalent t1 t2 @?= True

  , testCase "Inconsistent renaming fails" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          -- (λx. λy. x) ≢ (λx. λy. y)
          t1 = TAbs x (TAbs y (TVar x))
          t2 = TAbs x (TAbs y (TVar y))
      in alphaEquivalent t1 t2 @?= False

  , testCase "Application with alpha equivalence" $
      let x = Var "x"
          y = Var "y"
          -- (λx. x) (λy. y) ≈ (λx. x) (λx. x)
          t1 = TApp (TAbs x (TVar x)) (TAbs y (TVar y))
          t2 = TApp (TAbs x (TVar x)) (TAbs x (TVar x))
      in alphaEquivalent t1 t2 @?= True

  , testCase "Constant terms" $
      alphaEquivalent (TConst "5") (TConst "5") @?= True

  , testCase "Different constants" $
      alphaEquivalent (TConst "5") (TConst "6") @?= False
  ]

-- | Tests for eta reduction
etaReductionTests :: TestTree
etaReductionTests = testGroup "Eta Reduction"
  [ testCase "Eta-reducible abstraction" $
      let x = Var "x"
          f = Var "f"
          -- (λx. f x) where x not free in f
          term = TAbs x (TApp (TVar f) (TVar x))
      in etaReducible term @?= True

  , testCase "Eta reduction" $
      let x = Var "x"
          f = Var "f"
          -- (λx. f x) -> f
          term = TAbs x (TApp (TVar f) (TVar x))
      in etaReduce term @?= TVar f

  , testCase "Not eta-reducible when variable is free" $
      let x = Var "x"
          f = Var "f"
          -- (λx. x (f x)) - x is free in the argument
          term = TAbs x (TApp (TVar x) (TApp (TVar f) (TVar x)))
      in etaReducible term @?= False

  , testCase "Not eta-reducible when argument is not the variable" $
      let x = Var "x"
          y = Var "y"
          f = Var "f"
          -- (λx. f y)
          term = TAbs x (TApp (TVar f) (TVar y))
      in etaReducible term @?= False

  , testCase "Not eta-reducible for non-application body" $
      let x = Var "x"
      in etaReducible (TAbs x (TVar x)) @?= False
  ]

-- | Tests for normalization
normalizationTests :: TestTree
normalizationTests = testGroup "Normalization"
  [ testCase "Already normalized term" $
      let x = Var "x"
      in normalize (TVar x) @?= TVar x

  , testCase "Normalize beta redex" $
      let x = Var "x"
          -- (λx. x) 5 -> 5
          term = TApp (TAbs x (TVar x)) (TConst "5")
      in normalize term @?= TConst "5"

  , testCase "Idempotence of normalization" $
      let x = Var "x"
          term = TApp (TAbs x (TVar x)) (TConst "5")
          norm1 = normalize term
          norm2 = normalize norm1
      in norm1 @?= norm2

  , testCase "Nested applications" $
      let x = Var "x"
          y = Var "y"
          -- ((λx. λy. y) 5) 42 -> (λy. y) 42 -> 42
          term = TApp (TApp (TAbs x (TAbs y (TVar y))) (TConst "5")) (TConst "42")
      in normalize term @?= TConst "42"

  , testCase "Normalization with let-binding" $
      let x = Var "x"
          -- let x = 5 in x -> 5
          term = TLet x (TConst "5") (TVar x)
      in case normalize term of
           TConst "5" -> True @?= True
           _ -> False @?= True

  , testCase "Pair normalization" $
      let a = TConst "10"
          b = TConst "20"
      in normalize (TPair a b) @?= TPair a b
  ]

-- | Tests for definitional equality (the main API)
definitionalEqualityTests :: TestTree
definitionalEqualityTests = testGroup "Definitional Equality"
  [ testCase "Identity reduction" $
      let x = Var "x"
          -- (λx. x) 5 ≡ 5
          t1 = TApp (TAbs x (TVar x)) (TConst "5")
          t2 = TConst "5"
      in isDefinitionallyEqual t1 t2 @?= True

  , testCase "Alpha equivalence" $
      let x = Var "x"
          y = Var "y"
          -- (λx. x) ≡ (λy. y)
          t1 = TAbs x (TVar x)
          t2 = TAbs y (TVar y)
      in isDefinitionallyEqual t1 t2 @?= True

  , testCase "Reflexivity" $
      let x = Var "x"
          term = TVar x
      in isDefinitionallyEqual term term @?= True

  , testCase "Symmetry" $
      let t1 = TConst "5"
          t2 = TConst "5"
      in (isDefinitionallyEqual t1 t2) @?= (isDefinitionallyEqual t2 t1)

  , testCase "Transitivity" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          -- (λx. x) ≡ (λy. y) and (λy. y) ≡ (λz. z)
          t1 = TAbs x (TVar x)
          t2 = TAbs y (TVar y)
          t3 = TAbs z (TVar z)
      in (isDefinitionallyEqual t1 t2 && isDefinitionallyEqual t2 t3) @?=
         isDefinitionallyEqual t1 t3

  , testCase "Const equality" $
      let t1 = TConst "true"
          t2 = TConst "true"
      in isDefinitionallyEqual t1 t2 @?= True

  , testCase "Different constants" $
      let t1 = TConst "true"
          t2 = TConst "false"
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Chained beta reductions" $
      let x = Var "x"
          y = Var "y"
          -- ((λx. λy. y) 5) 42 ≡ 42
          t1 = TApp (TApp (TAbs x (TAbs y (TVar y))) (TConst "5")) (TConst "42")
          t2 = TConst "42"
      in isDefinitionallyEqual t1 t2 @?= True

  , testCase "Pair projections" $
      let -- fst (10, 20) ≡ 10
          t1 = TFst (TPair (TConst "10") (TConst "20"))
          t2 = TConst "10"
      in isDefinitionallyEqual t1 t2 @?= True
  ]

-- | NEGATIVE TESTS: Properties that should NOT be considered equal
-- These are critical to ensure the kernel doesn't accidentally prove algebraic theorems
negativeEqualityTests :: TestTree
negativeEqualityTests = testGroup "Negative Tests (Must Fail)"
  [ testCase "Commutativity is NOT definitional" $
      -- x + y should NOT equal y + x (requires explicit algebraic proof)
      -- We represent this as two applications for simplicity
      let x = Var "x"
          y = Var "y"
          -- TApp (TApp (TConst "+") x) y  ≢ TApp (TApp (TConst "+") y) x
          t1 = TApp (TApp (TConst "+") (TVar x)) (TVar y)
          t2 = TApp (TApp (TConst "+") (TVar y)) (TVar x)
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Different free variables are not equal" $
      let x = Var "x"
          y = Var "y"
      in isDefinitionallyEqual (TVar x) (TVar y) @?= False

  , testCase "Different abstractions over different variables" $
      let x = Var "x"
          y = Var "y"
          -- (λx. x + y) ≢ (λz. z + y) would be equivalent, but different variable names
          -- with different semantics (capture)
          -- Actually with proper alpha-equivalence, these should be equivalent
          -- Let me use a better example:
          -- (λx. x) ≢ (λx. y)
          t1 = TAbs x (TVar x)
          t2 = TAbs x (TVar y)
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Functions are not values" $
      let x = Var "x"
          -- (λx. x) ≢ 5
          t1 = TAbs x (TVar x)
          t2 = TConst "5"
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Associativity is NOT definitional" $
      -- (x + y) + z should NOT equal x + (y + z)
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          t1 = TApp (TApp (TConst "+") (TApp (TApp (TConst "+") (TVar x)) (TVar y))) (TVar z)
          t2 = TApp (TApp (TConst "+") (TVar x)) (TApp (TApp (TConst "+") (TVar y)) (TVar z))
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Distributivity is NOT definitional" $
      -- x * (y + z) should NOT equal (x * y) + (x * z)
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          t1 = TApp (TApp (TConst "*") (TVar x)) (TApp (TApp (TConst "+") (TVar y)) (TVar z))
          t2 = TApp (TApp (TConst "+") (TApp (TApp (TConst "*") (TVar x)) (TVar y)))
                     (TApp (TApp (TConst "*") (TVar x)) (TVar z))
      in isDefinitionallyEqual t1 t2 @?= False

  , testCase "Unequal constants" $
      isDefinitionallyEqual (TConst "1") (TConst "2") @?= False

  , testCase "Different applications are not equal" $
      let f = Var "f"
          x = Var "x"
          y = Var "y"
      in isDefinitionallyEqual (TApp (TVar f) (TVar x))
                              (TApp (TVar f) (TVar y)) @?= False
  ]

-- | Property-based tests for invariants
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "Normalization is idempotent" $
      \t -> normalize (normalize t) == normalize t

  , testProperty "Definitional equality is reflexive" $
      \t -> isDefinitionallyEqual t t

  , testProperty "Definitional equality is symmetric" $
      \t1 t2 -> isDefinitionallyEqual t1 t2 == isDefinitionallyEqual t2 t1

  , testProperty "Beta reduction preserves variables (in a sense)" $
      \x -> let term = TApp (TAbs x (TVar x)) (TVar x)
            in case betaReduce term of
                 TVar v -> v == x
                 _ -> False

  , testProperty "Convertibility is the same as definitional equality" $
      \t1 t2 -> isConvertible t1 t2 == isDefinitionallyEqual t1 t2

  , testProperty "Constant terms are only equal to themselves" $
      \c1 c2 -> c1 == c2 || not (isDefinitionallyEqual (TConst c1) (TConst c2))
  ]

-- QuickCheck Arbitrary instances for Term
instance Arbitrary Term where
  arbitrary = sized genTerm
    where
      genTerm 0 = oneof
        [ TVar <$> arbitrary
        , TConst <$> arbitrary
        ]
      genTerm n = oneof
        [ TVar <$> arbitrary
        , TConst <$> arbitrary
        , TAbs <$> arbitrary <*> genTerm (n `div` 2)
        , TApp <$> genTerm (n `div` 2) <*> genTerm (n `div` 2)
        , TLet <$> arbitrary <*> genTerm (n `div` 2) <*> genTerm (n `div` 2)
        , TPair <$> genTerm (n `div` 2) <*> genTerm (n `div` 2)
        , TFst <$> genTerm (n `div` 2)
        , TSnd <$> genTerm (n `div` 2)
        ]

instance Arbitrary Var where
  arbitrary = Var <$> elements ["x", "y", "z", "f", "g", "h", "a", "b", "c"]
