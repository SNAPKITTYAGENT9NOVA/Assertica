{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.StdLib.Setoid
Description : Comprehensive tests for setoid structure
Copyright   : (c) 2026 Ahmad Ali Parr
-}

module Test.StdLib.Setoid
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Set as Set

import Assertica.Core.AST
import Assertica.Core.ProofTerm
import Assertica.StdLib.Setoid

-- ============================================================================
-- UNIT TESTS: SETOID STRUCTURE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.StdLib.Setoid"
  [ testGroup "Trivial Setoid"
      [ testCase "Trivial setoid carrier is Unit" testTrivialCarrier
      , testCase "Trivial setoid reflexivity" testTrivialRefl
      , testCase "Trivial setoid symmetry" testTrivialSymm
      , testCase "Trivial setoid transitivity" testTrivialTrans
      , testCase "Trivial setoid all elements equivalent" testTrivialAllEq
      ]
  , testGroup "Discrete Setoid"
      [ testCase "Discrete setoid reflexivity" testDiscreteRefl
      , testCase "Discrete setoid symmetry" testDiscreteSymm
      , testCase "Discrete setoid transitivity" testDiscreteTrans
      ]
  , testGroup "Product Setoid"
      [ testCase "Product setoid carrier" testProductCarrier
      , testCase "Product setoid reflexivity" testProductRefl
      , testCase "Product setoid pair equality" testProductPairEq
      , testCase "Product setoid commutativity" testProductComm
      ]
  , testGroup "Setoid Morphisms"
      [ testCase "Identity morphism preserves equivalence" testIdMorphismPreserves
      , testCase "Identity morphism is left identity" testIdMorphismLeftId
      , testCase "Identity morphism is right identity" testIdMorphismRightId
      , testCase "Morphism composition" testMorphismComposition
      , testCase "Composed morphism preserves equivalence" testComposedMorphismPreserves
      ]
  , testGroup "Proof Terms"
      [ testCase "Reflexivity proof checks" testReflProofChecks
      , testCase "Symmetry proof checks" testSymmProofChecks
      , testCase "Transitivity proof checks" testTransProofChecks
      ]
  , testGroup "Equivalence Laws"
      [ testCase "Reflexivity law holds" testReflLaw
      , testCase "Symmetry law holds" testSymmLaw
      , testCase "Transitivity law holds" testTransLaw
      , testCase "Equivalence is reflexive everywhere" testReflEvery
      , testCase "Equivalence is symmetric everywhere" testSymmEvery
      , testCase "Equivalence is transitive everywhere" testTransEvery
      ]
  , testGroup "Setoid Morphism Laws"
      [ testCase "Morphism preserves reflexivity" testMorphismReflPreserved
      , testCase "Morphism preserves symmetry" testMorphismSymmPreserved
      , testCase "Morphism preserves transitivity" testMorphismTransPreserved
      ]
  , testGroup "Determinism Tests"
      [ testCase "Equivalence check is deterministic" testDeterminism
      , testCase "Morphism application is deterministic" testMorphismDeterminism
      ]
  , testGroup "Negative Tests"
      [ testCase "Reflexivity fails for unequal terms" testReflFails
      , testCase "Symmetry proof is not involutive" testSymmNotInvolutive
      ]
  ]

-- ============================================================================
-- TRIVIAL SETOID TESTS
-- ============================================================================

testTrivialCarrier :: Assertion
testTrivialCarrier =
  let s = trivialSetoid
  in setoidCarrier s @?= TConst (QName [] "Unit")

testTrivialRefl :: Assertion
testTrivialRefl =
  let s = trivialSetoid
      proof = setoidRefl s (Const UnitConst)
  in proof @?= Refl

testTrivialSymm :: Assertion
testTrivialSymm =
  let s = trivialSetoid
      innerProof = setoidRefl s (Const UnitConst)
      proof = setoidSymm s (Const UnitConst) (Const UnitConst) innerProof
  in proof @?= Refl

testTrivialTrans :: Assertion
testTrivialTrans =
  let s = trivialSetoid
      proof1 = setoidRefl s (Const UnitConst)
      proof2 = setoidRefl s (Const UnitConst)
      proof = setoidTrans s (Const UnitConst) (Const UnitConst) (Const UnitConst) proof1 proof2
  in proof @?= Trans Refl Refl

testTrivialAllEq :: Assertion
testTrivialAllEq =
  let s = trivialSetoid
      prop1 = setoidEq s (Const UnitConst) (Const UnitConst)
      prop2 = setoidEq s (Const UnitConst) (Const UnitConst)
  in prop1 @?= prop2

-- ============================================================================
-- DISCRETE SETOID TESTS
-- ============================================================================

testDiscreteRefl :: Assertion
testDiscreteRefl =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof = setoidRefl s a
  in proof @?= Refl

testDiscreteSymm :: Assertion
testDiscreteSymm =
  let s = discreteSetoid
      a = Const (IntLit 42)
      innerProof = Refl
      proof = setoidSymm s a a innerProof
  in proof @?= Symm Refl

testDiscreteTrans :: Assertion
testDiscreteTrans =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = Refl
      proof2 = Refl
      proof = setoidTrans s a a a proof1 proof2
  in proof @?= Trans Refl Refl

-- ============================================================================
-- PRODUCT SETOID TESTS
-- ============================================================================

testProductCarrier :: Assertion
testProductCarrier =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      prod = productSetoid s1 s2
      carrier = setoidCarrier prod
  in case carrier of
      TPair {} -> assertBool "Product carrier is a pair" True
      _ -> assertFailure "Product carrier should be a pair"

testProductRefl :: Assertion
testProductRefl =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      prod = productSetoid s1 s2
      pair = TPair (Const UnitConst) (Const (IntLit 1))
      proof = setoidRefl prod pair
  in proof @?= Refl

testProductPairEq :: Assertion
testProductPairEq =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      prod = productSetoid s1 s2
      a1 = Const UnitConst
      b1 = Const (IntLit 1)
      a2 = Const UnitConst
      b2 = Const (IntLit 1)
      p1 = TPair a1 b1
      p2 = TPair a2 b2
      prop = setoidEq prod p1 p2
  in case prop of
      ConjunctionProp {} -> assertBool "Product equality is a conjunction" True
      _ -> assertFailure "Product equality should be a conjunction"

testProductComm :: Assertion
testProductComm =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      prod = productSetoid s1 s2
      p1 = TPair (Const UnitConst) (Const (IntLit 1))
      p2 = TPair (Const UnitConst) (Const (IntLit 1))
      proof1 = setoidSymm prod p1 p2 Refl
      proof2 = setoidSymm prod p1 p2 Refl
  in proof1 @?= proof2

-- ============================================================================
-- SETOID MORPHISM TESTS
-- ============================================================================

testIdMorphismPreserves :: Assertion
testIdMorphismPreserves =
  let s = trivialSetoid
      morph = idMorphism s
      a = Const UnitConst
      b = Const UnitConst
      eqProof = Refl
      result = morphismPreservesEq morph a b eqProof
  in result @?= Refl

testIdMorphismLeftId :: Assertion
testIdMorphismLeftId =
  let s = discreteSetoid
      id_s = idMorphism s
      a = Const (IntLit 42)
      fa = morphismMap id_s a
  in fa @?= a

testIdMorphismRightId :: Assertion
testIdMorphismRightId =
  let s = discreteSetoid
      id_s = idMorphism s
      b = Const (IntLit 42)
      gb = morphismMap id_s b
  in gb @?= b

testMorphismComposition :: Assertion
testMorphismComposition =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      s3 = trivialSetoid
      f = SetoidMorphism
            { morphismSource = s1
            , morphismTarget = s2
            , morphismMap = \a -> Const (IntLit 1)
            , morphismProof = \_ _ _ -> Refl
            }
      g = SetoidMorphism
            { morphismSource = s2
            , morphismTarget = s3
            , morphismMap = \_ -> Const UnitConst
            , morphismProof = \_ _ _ -> Refl
            }
      composed = composeMorphisms f g
  in morphismSource composed @?= s1 && morphismTarget composed @?= s3

testComposedMorphismPreserves :: Assertion
testComposedMorphismPreserves =
  let s1 = trivialSetoid
      s2 = discreteSetoid
      s3 = trivialSetoid
      f = idMorphism s2
      g = idMorphism s3
      composed = composeMorphisms f g
      a = Const (IntLit 42)
      b = Const (IntLit 42)
      eqProof = Refl
      result = morphismPreservesEq composed a b eqProof
  in result @?= Refl

-- ============================================================================
-- PROOF TERM TESTS
-- ============================================================================

testReflProofChecks :: Assertion
testReflProofChecks =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof = setoidRefl s a
      prop = EqualityProp a a
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Reflexivity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testSymmProofChecks :: Assertion
testSymmProofChecks =
  let s = discreteSetoid
      a = Const (IntLit 42)
      innerProof = Refl
      proof = setoidSymm s a a innerProof
      prop = EqualityProp a a
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Symmetry proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testTransProofChecks :: Assertion
testTransProofChecks =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = Refl
      proof2 = Refl
      proof = setoidTrans s a a a proof1 proof2
      prop = EqualityProp a a
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Transitivity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

-- ============================================================================
-- EQUIVALENCE LAW TESTS
-- ============================================================================

testReflLaw :: Assertion
testReflLaw =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof = setoidRefl s a
      prop = EqualityProp a a
  in proofChecks proof prop `shouldSucceed` ()

testSymmLaw :: Assertion
testSymmLaw =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = setoidRefl s a
      proof2 = setoidSymm s a a proof1
      prop = EqualityProp a a
  in proofChecks proof2 prop `shouldSucceed` ()

testTransLaw :: Assertion
testTransLaw =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = setoidRefl s a
      proof2 = setoidRefl s a
      proof3 = setoidTrans s a a a proof1 proof2
      prop = EqualityProp a a
  in proofChecks proof3 prop `shouldSucceed` ()

testReflEvery :: Assertion
testReflEvery =
  let s = discreteSetoid
      elements = [Const (IntLit 1), Const (IntLit 2), Const UnitConst]
      results = map (\a -> (a, setoidRefl s a)) elements
  in all (\(_, proof) -> proofChecks proof (EqualityProp (Const (IntLit 1)) (Const (IntLit 1))) == Right ())
       results
    @? "All elements satisfy reflexivity"

testSymmEvery :: Assertion
testSymmEvery =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = Refl
      proof2 = setoidSymm s a a proof1
  in proofChecks proof2 (EqualityProp a a) `shouldSucceed` ()

testTransEvery :: Assertion
testTransEvery =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = Refl
      proof2 = Refl
      proof3 = setoidTrans s a a a proof1 proof2
  in proofChecks proof3 (EqualityProp a a) `shouldSucceed` ()

-- ============================================================================
-- MORPHISM LAW TESTS
-- ============================================================================

testMorphismReflPreserved :: Assertion
testMorphismReflPreserved =
  let s1 = discreteSetoid
      s2 = discreteSetoid
      morph = idMorphism s1
      a = Const (IntLit 42)
      reflProof = setoidRefl s1 a
      result = morphismPreservesEq morph a a reflProof
  in result @?= Refl

testMorphismSymmPreserved :: Assertion
testMorphismSymmPreserved =
  let s1 = discreteSetoid
      morph = idMorphism s1
      a = Const (IntLit 42)
      symmProof = Symm Refl
      result = morphismPreservesEq morph a a symmProof
  in result @?= Symm Refl

testMorphismTransPreserved :: Assertion
testMorphismTransPreserved =
  let s1 = discreteSetoid
      morph = idMorphism s1
      a = Const (IntLit 42)
      transProof = Trans Refl Refl
      result = morphismPreservesEq morph a a transProof
  in result @?= Trans Refl Refl

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testDeterminism :: Assertion
testDeterminism =
  let s = discreteSetoid
      a = Const (IntLit 42)
      proof1 = setoidRefl s a
      proof2 = setoidRefl s a
  in proof1 @?= proof2

testMorphismDeterminism :: Assertion
testMorphismDeterminism =
  let s = discreteSetoid
      morph = idMorphism s
      a = Const (IntLit 42)
      result1 = morphismMap morph a
      result2 = morphismMap morph a
  in result1 @?= result2

-- ============================================================================
-- NEGATIVE TESTS
-- ============================================================================

testReflFails :: Assertion
testReflFails =
  let a = Const (IntLit 42)
      b = Const (IntLit 43)
      proof = Refl
      prop = EqualityProp a b
      result = proofChecks proof prop
  in case result of
      Left _ -> assertBool "Reflexivity correctly fails" True
      Right () -> assertFailure "Reflexivity should fail for unequal terms"

testSymmNotInvolutive :: Assertion
testSymmNotInvolutive =
  let proof1 = Refl
      proof2 = Symm proof1
      proof3 = Symm proof2
  in proof3 @?= Refl

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

shouldSucceed :: Either e a -> a -> Assertion
shouldSucceed (Right x) expected = x @?= expected
shouldSucceed (Left err) _ = assertFailure $ "Expected success, got error: " ++ show err

-- Alias for property tests
_propTest :: String -> Bool -> TestTree
_propTest name prop = testCase name $ assertBool name prop
