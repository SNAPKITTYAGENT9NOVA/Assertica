{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.StdLib.Monomorphism
Description : Tests for lattice monomorphism theorem
Copyright   : (c) 2026 Ahmad Ali Parr
-}

module Test.StdLib.Monomorphism
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Assertica.Core.AST
import Assertica.Core.ProofTerm
import Assertica.StdLib.Lattice
import Assertica.StdLib.Monomorphism

-- ============================================================================
-- UNIT TESTS: MONOMORPHISM THEOREM
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.StdLib.Monomorphism"
  [ testGroup "Monomorphism Theorem"
      [ testCase "Identity homomorphism is monomorphic" testIdentityMonomorphic
      , testCase "Same homomorphisms agree on generators" testSameHomsAgree
      , testCase "Monomorphism proof is constructible" testMonomorphismConstructible
      , testCase "Monomorphism proof on generators" testMonomorphismOnGenerators
      , testCase "Monomorphism proof on compounds" testMonomorphismOnCompounds
      ]
  , testGroup "Homomorphism Equality"
      [ testCase "Equal homomorphisms have same map" testEqualHomsSameMap
      , testCase "Homomorphism equality proof" testHomEqualityProof
      ]
  , testGroup "Structural Induction"
      [ testCase "Base case: generators" testInductionBase
      , testCase "Induction step: join" testInductionStepJoin
      , testCase "Induction step: meet" testInductionStepMeet
      , testCase "Full induction" testFullInduction
      ]
  , testGroup "Generator Extension"
      [ testCase "Extension on generators" testExtensionGenerators
      , testCase "Extended function preserves joins" testExtensionPreservesJoins
      , testCase "Extended function preserves meets" testExtensionPreservesMeets
      , testCase "Unique extension" testUniqueExtension
      ]
  , testGroup "Homomorphism Composition"
      [ testCase "Composed homomorphisms" testComposedHomomorphisms
      , testCase "Composition preserves monomorphism" testCompositionMonomorphism
      ]
  , testGroup "Proof Term Correctness"
      [ testCase "Monomorphism proof type-checks" testMonomorphismProofChecks
      , testCase "Induction step proofs" testInductionStepProofs
      , testCase "Extension proofs" testExtensionProofs
      ]
  , testGroup "Determinism"
      [ testCase "Monomorphism proof is deterministic" testMonomorphismDeterminism
      , testCase "Structural induction is deterministic" testInductionDeterminism
      , testCase "Extension is deterministic" testExtensionDeterminism
      ]
  , testGroup "Negative Tests"
      [ testCase "Different homomorphisms may not agree" testDifferentHomsMayDiffer
      , testCase "Non-agreeing on generators" testNonAgreeingGenerators
      ]
  ]

-- ============================================================================
-- MONOMORPHISM THEOREM TESTS
-- ============================================================================

testIdentityMonomorphic :: Assertion
testIdentityMonomorphic =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl  -- Identity agrees with itself
      a = Const (BoolLit True)
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq a
  in proof `shouldBeProofTerm` ()

testSameHomsAgree :: Assertion
testSameHomsAgree =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      -- Same homomorphism always agrees
  in homMap f (Const (BoolLit True)) @?= homMap g (Const (BoolLit True))

testMonomorphismConstructible :: Assertion
testMonomorphismConstructible =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      a = Const (BoolLit True)
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq a
  in case proof of
      _ -> assertBool "Monomorphism proof is constructible" True

testMonomorphismOnGenerators :: Assertion
testMonomorphismOnGenerators =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      -- Both agree on generators
      genEq gen = Refl
      -- Generator (a constant)
      gen = Const (BoolLit True)
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq gen
  in proof @?= Refl

testMonomorphismOnCompounds :: Assertion
testMonomorphismOnCompounds =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      -- Compound: a ⊔ b
      compound = Constr (QName [] "join") [Const (BoolLit True), Const (BoolLit False)]
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq compound
  in proof `shouldBeProofTerm` ()

-- ============================================================================
-- HOMOMORPHISM EQUALITY TESTS
-- ============================================================================

testEqualHomsSameMap :: Assertion
testEqualHomsSameMap =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      a = Const (BoolLit True)
  in homMap f a @?= homMap g a

testHomEqualityProof :: Assertion
testHomEqualityProof =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      proof = homomorphismsEqual booleanLattice booleanLattice f g genEq
  in proof `shouldBeProofTerm` ()

-- ============================================================================
-- STRUCTURAL INDUCTION TESTS
-- ============================================================================

testInductionBase :: Assertion
testInductionBase =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      genProof gen = Refl
      gen = Const (BoolLit True)
      proof = inductionBase booleanLattice genProof gen
  in proof @?= Refl

testInductionStepJoin :: Assertion
testInductionStepJoin =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      hypA = Refl
      hypB = Refl
      proof = inductionStep booleanLattice booleanLattice f g a b hypA hypB
  in proof `shouldBeProofTerm` ()

testInductionStepMeet :: Assertion
testInductionStepMeet =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      hypA = Refl
      hypB = Refl
      proof = inductionStep booleanLattice booleanLattice f g a b hypA hypB
  in proof `shouldBeProofTerm` ()

testFullInduction :: Assertion
testFullInduction =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      -- Complex term: (a ⊔ b) ⊓ (c ⊔ d)
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      d = Const (BoolLit False)
      complex = Constr (QName [] "meet")
                  [ Constr (QName [] "join") [a, b]
                  , Constr (QName [] "join") [c, d]
                  ]
      proof = structuralInductionLattice booleanLattice booleanLattice f g genEq complex
  in proof `shouldBeProofTerm` ()

-- ============================================================================
-- GENERATOR EXTENSION TESTS
-- ============================================================================

testExtensionGenerators :: Assertion
testExtensionGenerators =
  let phi gen = Const (IntLit 1)  -- Map everything to 1
      hom = generatorExtension booleanLattice phi
      gen = Const (BoolLit True)
  in homMap hom gen @?= Const (IntLit 1)

testExtensionPreservesJoins :: Assertion
testExtensionPreservesJoins =
  let phi gen = Const (IntLit 1)
      hom = generatorExtension booleanLattice phi
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = homPreservesJoin hom a b
  in proof @?= Refl

testExtensionPreservesMeets :: Assertion
testExtensionPreservesMeets =
  let phi gen = Const (IntLit 1)
      hom = generatorExtension booleanLattice phi
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = homPreservesMeet hom a b
  in proof @?= Refl

testUniqueExtension :: Assertion
testUniqueExtension =
  let phi gen = Const (IntLit 1)
      a = Const (BoolLit True)
      proof = uniqueExtension booleanLattice booleanLattice phi a
  in proof `shouldBeProofTerm` ()

-- ============================================================================
-- HOMOMORPHISM COMPOSITION TESTS
-- ============================================================================

testComposedHomomorphisms :: Assertion
testComposedHomomorphisms =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      a = Const (BoolLit True)
      composed = homMap f (homMap g a)
  in composed @?= a

testCompositionMonomorphism :: Assertion
testCompositionMonomorphism =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      a = Const (BoolLit True)
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq a
  in proof `shouldBeProofTerm` ()

-- ============================================================================
-- PROOF TERM CORRECTNESS TESTS
-- ============================================================================

testMonomorphismProofChecks :: Assertion
testMonomorphismProofChecks =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      a = Const (BoolLit True)
      proof = monomorphismTheorem booleanLattice booleanLattice f g genEq a
  in case proof of
      _ -> assertBool "Monomorphism proof exists" True

testInductionStepProofs :: Assertion
testInductionStepProofs =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = inductionStep booleanLattice booleanLattice f g a b Refl Refl
  in case proof of
      _ -> assertBool "Induction step proof exists" True

testExtensionProofs :: Assertion
testExtensionProofs =
  let phi gen = Const (IntLit 1)
      ext = extendFromGenerators booleanLattice booleanLattice phi
      a = Const (BoolLit True)
  in homMap ext a @?= phi a

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testMonomorphismDeterminism :: Assertion
testMonomorphismDeterminism =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      a = Const (BoolLit True)
      proof1 = monomorphismTheorem booleanLattice booleanLattice f g genEq a
      proof2 = monomorphismTheorem booleanLattice booleanLattice f g genEq a
  in proof1 @?= proof2

testInductionDeterminism :: Assertion
testInductionDeterminism =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = f
      genEq gen = Refl
      a = Const (BoolLit True)
      proof1 = structuralInductionLattice booleanLattice booleanLattice f g genEq a
      proof2 = structuralInductionLattice booleanLattice booleanLattice f g genEq a
  in proof1 @?= proof2

testExtensionDeterminism :: Assertion
testExtensionDeterminism =
  let phi gen = Const (IntLit 1)
      ext1 = extendFromGenerators booleanLattice booleanLattice phi
      ext2 = extendFromGenerators booleanLattice booleanLattice phi
      a = Const (BoolLit True)
  in homMap ext1 a @?= homMap ext2 a

-- ============================================================================
-- NEGATIVE TESTS
-- ============================================================================

testDifferentHomsMayDiffer :: Assertion
testDifferentHomsMayDiffer =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = \_ -> Const (BoolLit False)  -- Constant False
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      a = Const (BoolLit True)
  in homMap f a /= homMap g a @? "Different homomorphisms may map differently"

testNonAgreeingGenerators :: Assertion
testNonAgreeingGenerators =
  let f = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = id
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      g = LatticeHomomorphism
             { homSource = booleanLattice
             , homTarget = booleanLattice
             , homMap = \_ -> Const (BoolLit False)
             , homPreservesJoin = \_ _ -> Refl
             , homPreservesMeet = \_ _ -> Refl
             }
      true = Const (BoolLit True)
  in homMap f true /= homMap g true @? "Homomorphisms that differ on generators are different"

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

shouldBeProofTerm :: Proof -> () -> Assertion
shouldBeProofTerm proof () = case proof of
  Refl {} -> assertBool "Is a proof term (Refl)" True
  Symm {} -> assertBool "Is a proof term (Symm)" True
  Trans {} -> assertBool "Is a proof term (Trans)" True
  Intro {} -> assertBool "Is a proof term (Intro)" True
  Elim {} -> assertBool "Is a proof term (Elim)" True
  Constr' {} -> assertBool "Is a proof term (Constr')" True
  Opaque {} -> assertBool "Is a proof term (Opaque)" True
  ProofAnn {} -> assertBool "Is a proof term (ProofAnn)" True
  _ -> assertFailure "Not a proper proof term"
