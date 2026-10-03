{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.StdLib.Lattice
Description : Comprehensive tests for lattice algebraic structure
Copyright   : (c) 2026 Ahmad Ali Parr
-}

module Test.StdLib.Lattice
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Assertica.Core.AST
import Assertica.Core.ProofTerm
import Assertica.StdLib.Setoid
import Assertica.StdLib.Lattice

-- ============================================================================
-- UNIT TESTS: LATTICE STRUCTURE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.StdLib.Lattice"
  [ testGroup "Boolean Lattice"
      [ testCase "Boolean lattice carrier" testBooleanCarrier
      , testCase "Boolean join operation" testBooleanJoin
      , testCase "Boolean meet operation" testBooleanMeet
      , testCase "Boolean join reflexivity" testBooleanJoinRefl
      , testCase "Boolean meet reflexivity" testBooleanMeetRefl
      ]
  , testGroup "Trivial Lattice"
      [ testCase "Trivial lattice carrier" testTrivialLatticeCarrier
      , testCase "Trivial lattice join" testTrivialLatticeJoin
      , testCase "Trivial lattice meet" testTrivialLatticeMeet
      ]
  , testGroup "Join Operation Properties"
      [ testCase "Join associativity" testJoinAssociativity
      , testCase "Join commutativity" testJoinCommutativity
      , testCase "Join idempotence" testJoinIdempotence
      , testCase "Join preserves equivalence (left)" testJoinPreservesEqLeft
      , testCase "Join preserves equivalence (right)" testJoinPreservesEqRight
      ]
  , testGroup "Meet Operation Properties"
      [ testCase "Meet associativity" testMeetAssociativity
      , testCase "Meet commutativity" testMeetCommutativity
      , testCase "Meet idempotence" testMeetIdempotence
      , testCase "Meet preserves equivalence (left)" testMeetPreservesEqLeft
      , testCase "Meet preserves equivalence (right)" testMeetPreservesEqRight
      ]
  , testGroup "Lattice Homomorphisms"
      [ testCase "Identity homomorphism" testIdentityHomomorphism
      , testCase "Identity preserves join" testIdPreservesJoin
      , testCase "Identity preserves meet" testIdPreservesMeet
      , testCase "Homomorphism composition" testHomomorphismComposition
      ]
  , testGroup "Proof Term Checking"
      [ testCase "Join associativity proof checks" testJoinAssocProofChecks
      , testCase "Join commutativity proof checks" testJoinCommProofChecks
      , testCase "Join idempotence proof checks" testJoinIdempProofChecks
      , testCase "Meet associativity proof checks" testMeetAssocProofChecks
      , testCase "Meet commutativity proof checks" testMeetCommProofChecks
      , testCase "Meet idempotence proof checks" testMeetIdempProofChecks
      ]
  , testGroup "Lattice Laws Interplay"
      [ testCase "Join and meet compatibility" testJoinMeetCompat
      , testCase "Join properties hold simultaneously" testJoinPropsSimultaneous
      , testCase "Meet properties hold simultaneously" testMeetPropsSimultaneous
      ]
  , testGroup "Determinism Tests"
      [ testCase "Join operation is deterministic" testJoinDeterminism
      , testCase "Meet operation is deterministic" testMeetDeterminism
      , testCase "Proof generation is deterministic" testProofDeterminism
      ]
  , testGroup "Negative Tests"
      [ testCase "Different elements don't unify wrongly" testElementsDifferent
      ]
  ]

-- ============================================================================
-- BOOLEAN LATTICE TESTS
-- ============================================================================

testBooleanCarrier :: Assertion
testBooleanCarrier =
  let carrier = latticeCarrier booleanLattice
      setoidCarrier' = setoidCarrier carrier
  in case setoidCarrier' of
      TConst (QName [] "Bool") -> assertBool "Boolean lattice carrier is Bool" True
      _ -> assertFailure "Boolean lattice carrier should be Bool"

testBooleanJoin :: Assertion
testBooleanJoin =
  let result = join booleanLattice (Const (BoolLit True)) (Const (BoolLit False))
  in case result of
      Constr (QName [] "or") [_, _] -> assertBool "Join creates OR constructor" True
      _ -> assertFailure "Join should create 'or' constructor"

testBooleanMeet :: Assertion
testBooleanMeet =
  let result = meet booleanLattice (Const (BoolLit True)) (Const (BoolLit False))
  in case result of
      Constr (QName [] "and") [_, _] -> assertBool "Meet creates AND constructor" True
      _ -> assertFailure "Meet should create 'and' constructor"

testBooleanJoinRefl :: Assertion
testBooleanJoinRefl =
  let proof = joinAssociativity booleanLattice
                 (Const (BoolLit True))
                 (Const (BoolLit False))
                 (Const (BoolLit True))
  in proof @?= Refl

testBooleanMeetRefl :: Assertion
testBooleanMeetRefl =
  let proof = meetAssociativity booleanLattice
                 (Const (BoolLit True))
                 (Const (BoolLit False))
                 (Const (BoolLit True))
  in proof @?= Refl

-- ============================================================================
-- TRIVIAL LATTICE TESTS
-- ============================================================================

testTrivialLatticeCarrier :: Assertion
testTrivialLatticeCarrier =
  let carrier = latticeCarrier trivialLattice
      setoidCarrier' = setoidCarrier carrier
  in case setoidCarrier' of
      TConst (QName [] "Unit") -> assertBool "Trivial lattice carrier is Unit" True
      _ -> assertFailure "Trivial lattice carrier should be Unit"

testTrivialLatticeJoin :: Assertion
testTrivialLatticeJoin =
  let result = join trivialLattice (Const UnitConst) (Const UnitConst)
  in result @?= Const UnitConst

testTrivialLatticeMeet :: Assertion
testTrivialLatticeMeet =
  let result = meet trivialLattice (Const UnitConst) (Const UnitConst)
  in result @?= Const UnitConst

-- ============================================================================
-- JOIN OPERATION TESTS
-- ============================================================================

testJoinAssociativity :: Assertion
testJoinAssociativity =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      proof = joinAssociativity booleanLattice a b c
  in proof @?= Refl

testJoinCommutativity :: Assertion
testJoinCommutativity =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = joinCommutativity booleanLattice a b
  in proof @?= Refl

testJoinIdempotence :: Assertion
testJoinIdempotence =
  let a = Const (BoolLit True)
      proof = joinIdempotence booleanLattice a
  in proof @?= Refl

testJoinPreservesEqLeft :: Assertion
testJoinPreservesEqLeft =
  let a = Const (BoolLit True)
      a' = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = preserveJoin booleanLattice a a' b b Refl Refl
  in proof @?= Refl

testJoinPreservesEqRight :: Assertion
testJoinPreservesEqRight =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      b' = Const (BoolLit False)
      proof = preserveJoin booleanLattice a a b b' Refl Refl
  in proof @?= Refl

-- ============================================================================
-- MEET OPERATION TESTS
-- ============================================================================

testMeetAssociativity :: Assertion
testMeetAssociativity =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      proof = meetAssociativity booleanLattice a b c
  in proof @?= Refl

testMeetCommutativity :: Assertion
testMeetCommutativity =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = meetCommutativity booleanLattice a b
  in proof @?= Refl

testMeetIdempotence :: Assertion
testMeetIdempotence =
  let a = Const (BoolLit True)
      proof = meetIdempotence booleanLattice a
  in proof @?= Refl

testMeetPreservesEqLeft :: Assertion
testMeetPreservesEqLeft =
  let a = Const (BoolLit True)
      a' = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = preserveMeet booleanLattice a a' b b Refl Refl
  in proof @?= Refl

testMeetPreservesEqRight :: Assertion
testMeetPreservesEqRight =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      b' = Const (BoolLit False)
      proof = preserveMeet booleanLattice a a b b' Refl Refl
  in proof @?= Refl

-- ============================================================================
-- LATTICE HOMOMORPHISM TESTS
-- ============================================================================

testIdentityHomomorphism :: Assertion
testIdentityHomomorphism =
  let hom = LatticeHomomorphism
              { homSource = booleanLattice
              , homTarget = booleanLattice
              , homMap = id
              , homPreservesJoin = \_ _ -> Refl
              , homPreservesMeet = \_ _ -> Refl
              }
      a = Const (BoolLit True)
  in homMap hom a @?= a

testIdPreservesJoin :: Assertion
testIdPreservesJoin =
  let hom = LatticeHomomorphism
              { homSource = booleanLattice
              , homTarget = booleanLattice
              , homMap = id
              , homPreservesJoin = \_ _ -> Refl
              , homPreservesMeet = \_ _ -> Refl
              }
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = homPreservesJoin hom a b
  in proof @?= Refl

testIdPreservesMeet :: Assertion
testIdPreservesMeet =
  let hom = LatticeHomomorphism
              { homSource = booleanLattice
              , homTarget = booleanLattice
              , homMap = id
              , homPreservesJoin = \_ _ -> Refl
              , homPreservesMeet = \_ _ -> Refl
              }
      a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = homPreservesMeet hom a b
  in proof @?= Refl

testHomomorphismComposition :: Assertion
testHomomorphismComposition =
  let hom1 = LatticeHomomorphism
               { homSource = booleanLattice
               , homTarget = booleanLattice
               , homMap = id
               , homPreservesJoin = \_ _ -> Refl
               , homPreservesMeet = \_ _ -> Refl
               }
      hom2 = LatticeHomomorphism
               { homSource = booleanLattice
               , homTarget = booleanLattice
               , homMap = id
               , homPreservesJoin = \_ _ -> Refl
               , homPreservesMeet = \_ _ -> Refl
               }
      a = Const (BoolLit True)
      composed = homMap hom1 (homMap hom2 a)
  in composed @?= a

-- ============================================================================
-- PROOF TERM CHECKING TESTS
-- ============================================================================

testJoinAssocProofChecks :: Assertion
testJoinAssocProofChecks =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      proof = joinAssociativity booleanLattice a b c
      -- (a ⊔ b) ⊔ c ≈ a ⊔ (b ⊔ c)
      lhs = join booleanLattice (join booleanLattice a b) c
      rhs = join booleanLattice a (join booleanLattice b c)
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Join associativity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testJoinCommProofChecks :: Assertion
testJoinCommProofChecks =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = joinCommutativity booleanLattice a b
      -- a ⊔ b ≈ b ⊔ a
      lhs = join booleanLattice a b
      rhs = join booleanLattice b a
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Join commutativity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testJoinIdempProofChecks :: Assertion
testJoinIdempProofChecks =
  let a = Const (BoolLit True)
      proof = joinIdempotence booleanLattice a
      -- a ⊔ a ≈ a
      lhs = join booleanLattice a a
      rhs = a
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Join idempotence proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testMeetAssocProofChecks :: Assertion
testMeetAssocProofChecks =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      proof = meetAssociativity booleanLattice a b c
      -- (a ⊓ b) ⊓ c ≈ a ⊓ (b ⊓ c)
      lhs = meet booleanLattice (meet booleanLattice a b) c
      rhs = meet booleanLattice a (meet booleanLattice b c)
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Meet associativity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testMeetCommProofChecks :: Assertion
testMeetCommProofChecks =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof = meetCommutativity booleanLattice a b
      -- a ⊓ b ≈ b ⊓ a
      lhs = meet booleanLattice a b
      rhs = meet booleanLattice b a
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Meet commutativity proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

testMeetIdempProofChecks :: Assertion
testMeetIdempProofChecks =
  let a = Const (BoolLit True)
      proof = meetIdempotence booleanLattice a
      -- a ⊓ a ≈ a
      lhs = meet booleanLattice a a
      rhs = a
      prop = EqualityProp lhs rhs
      result = proofChecks proof prop
  in case result of
      Right () -> assertBool "Meet idempotence proof checks" True
      Left err -> assertFailure $ "Proof should check: " ++ show err

-- ============================================================================
-- LATTICE LAWS INTERPLAY TESTS
-- ============================================================================

testJoinMeetCompat :: Assertion
testJoinMeetCompat =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      join_result = join booleanLattice a b
      meet_result = meet booleanLattice a b
  in (join_result /= meet_result) @? "Join and meet produce different results"

testJoinPropsSimultaneous :: Assertion
testJoinPropsSimultaneous =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      assoc = joinAssociativity booleanLattice a b c
      comm = joinCommutativity booleanLattice a b
      idemp = joinIdempotence booleanLattice a
  in (assoc @?= Refl) >> (comm @?= Refl) >> (idemp @?= Refl)

testMeetPropsSimultaneous :: Assertion
testMeetPropsSimultaneous =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      c = Const (BoolLit True)
      assoc = meetAssociativity booleanLattice a b c
      comm = meetCommutativity booleanLattice a b
      idemp = meetIdempotence booleanLattice a
  in (assoc @?= Refl) >> (comm @?= Refl) >> (idemp @?= Refl)

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testJoinDeterminism :: Assertion
testJoinDeterminism =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      result1 = join booleanLattice a b
      result2 = join booleanLattice a b
  in result1 @?= result2

testMeetDeterminism :: Assertion
testMeetDeterminism =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      result1 = meet booleanLattice a b
      result2 = meet booleanLattice a b
  in result1 @?= result2

testProofDeterminism :: Assertion
testProofDeterminism =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
      proof1 = joinCommutativity booleanLattice a b
      proof2 = joinCommutativity booleanLattice a b
  in proof1 @?= proof2

-- ============================================================================
-- NEGATIVE TESTS
-- ============================================================================

testElementsDifferent :: Assertion
testElementsDifferent =
  let a = Const (BoolLit True)
      b = Const (BoolLit False)
  in a /= b @? "Different boolean values should not be equal"
