{-|
Module      : Test.Core.Assertion
Description : Tests for the explicit assertion system

Tests the assertion module which represents mathematical obligations
as first-class program entities.
-}

module Test.Core.Assertion (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Assertica.Core.AST
import Assertica.Core.Assertion

tests :: TestTree
tests = testGroup "Assertica.Core.Assertion"
  [ assertionConstructionTests
  , obligationStoreTests
  , propositionUtilityTests
  ]

-- Tests for assertion construction
assertionConstructionTests :: TestTree
assertionConstructionTests = testGroup "Assertion Construction"
  [ testCase "Create a simple assertion" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TVar x) (TVar y)
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "eq_test" prop loc
      in assertionName assertion @?= "eq_test"

  , testCase "Assertion maintains proposition" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "const_eq" prop loc
      in assertionProp assertion @?= prop

  , testCase "Assertion maintains source location" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 42 7
          assertion = mkAssertion "test" prop loc
      in assertionLoc assertion @?= loc
  ]

-- Tests for obligation store operations
obligationStoreTests :: TestTree
obligationStoreTests = testGroup "Obligation Store"
  [ testCase "Empty store is empty" $
      length (getObligations emptyStore) @?= 0

  , testCase "Insert obligation into store" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test1" prop loc
          store = insertObligation assertion emptyStore
      in length (getObligations store) @?= 1

  , testCase "Look up obligation by name" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_lookup" prop loc
          store = insertObligation assertion emptyStore
      in case lookupObligation "test_lookup" store of
           Just ob -> obligationState ob @?= Asserted
           Nothing -> False @?= True

  , testCase "Obligation starts in Asserted state" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_asserted" prop loc
          store = insertObligation assertion emptyStore
      in case lookupObligation "test_asserted" store of
           Just ob -> obligationState ob @?= Asserted
           Nothing -> False @?= True

  , testCase "Mark obligation as proven" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_proven" prop loc
          store = insertObligation assertion emptyStore
      in case markProven "test_proven" (Just "proof_term_1") store of
           Right store' ->
             case lookupObligation "test_proven" store' of
               Just ob -> obligationState ob @?= Proven
               Nothing -> False @?= True
           Left _ -> False @?= True

  , testCase "Mark obligation as unproven" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_unproven" prop loc
          store = insertObligation assertion emptyStore
      in case markUnproven "test_unproven" store of
           Right store' ->
             case lookupObligation "test_unproven" store' of
               Just ob -> obligationState ob @?= Unproven
               Nothing -> False @?= True
           Left _ -> False @?= True

  , testCase "Check allProven when all proven" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_all_proven" prop loc
          store = insertObligation assertion emptyStore
      in case markProven "test_all_proven" Nothing store of
           Right store' -> allProven store' @?= True
           Left _ -> False @?= True

  , testCase "Check allProven when some unproven" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TConst "5")
          prop2 = EqualityProp (TConst "1") (TConst "1")
          loc = SourceLoc "test.assertica" 1 1
          assertion1 = mkAssertion "test_1" prop1 loc
          assertion2 = mkAssertion "test_2" prop2 loc
          store = insertObligation assertion1 $ insertObligation assertion2 emptyStore
      in allProven store @?= False

  , testCase "Count obligations by state" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          loc = SourceLoc "test.assertica" 1 1
          assertion = mkAssertion "test_count" prop loc
          store = insertObligation assertion emptyStore
      in countByState Asserted store @?= 1
  ]

-- Tests for proposition utilities
propositionUtilityTests :: TestTree
propositionUtilityTests = testGroup "Proposition Utilities"
  [ testCase "Pretty print equality proposition" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
      in length (prettyPrintProposition prop) > 0 @?= True

  , testCase "Pretty print universal quantification" $
      let x = Var "x"
          innerProp = EqualityProp (TVar x) (TConst "5")
          prop = UniversalQuantProp x innerProp
      in length (prettyPrintProposition prop) > 0 @?= True

  , testCase "Pretty print implication" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TConst "5")
          prop2 = EqualityProp (TConst "1") (TConst "1")
          prop = ImplicationProp prop1 prop2
      in length (prettyPrintProposition prop) > 0 @?= True

  , testCase "Pretty print conjunction" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TConst "5")
          prop2 = EqualityProp (TConst "1") (TConst "1")
          prop = ConjunctionProp prop1 prop2
      in length (prettyPrintProposition prop) > 0 @?= True

  , testCase "Pretty print disjunction" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TConst "5")
          prop2 = EqualityProp (TConst "1") (TConst "1")
          prop = DisjunctionProp prop1 prop2
      in length (prettyPrintProposition prop) > 0 @?= True

  , testCase "Pretty print negation" $
      let x = Var "x"
          prop = NegationProp (EqualityProp (TVar x) (TConst "5"))
      in length (prettyPrintProposition prop) > 0 @?= True
  ]
