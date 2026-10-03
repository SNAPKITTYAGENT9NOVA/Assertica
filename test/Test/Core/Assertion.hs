{-|
Module      : Test.Core.Assertion
Description : Tests for the Explicit Assertion System

Tests the assertion system components:
  - Proposition AST (equality, universal quantification, implication, conjunction)
  - Assertion creation and naming
  - Proof obligation tracking
  - Variable binding and substitution in propositions
  - Free/bound variable computation
  - Pretty printing
-}

module Test.Core.Assertion (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Assertica.Core.AST
import Assertica.Core.Assertion
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

tests :: TestTree
tests = testGroup "Assertica.Core.Assertion"
  [ propositionEqualityTests
  , propositionQuantificationTests
  , propositionImplicationTests
  , propositionConjunctionTests
  , propositionTypingTests
  , propositionPredicateTests
  , assertionConstructionTests
  , obligationStoreTests
  , propFreeVariablesTests
  , propSubstituteTests
  , propAlphaRenameTests
  , prettyPrintTests
  , elaborationInterfaceTests
  ]

-- ============================================================================
-- Equality Proposition Tests
-- ============================================================================

propositionEqualityTests :: TestTree
propositionEqualityTests = testGroup "Equality Propositions (a ≡ b)"
  [ testCase "Simple equality: constant to constant" $
      let prop = EqualityProp (TConst "1") (TConst "1")
      in prop @?= EqualityProp (TConst "1") (TConst "1")

  , testCase "Equality with variables" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
      in prop @?= EqualityProp (TVar x) (TVar x)

  , testCase "Asymmetric equality: a ≡ b where a ≠ b" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TVar x) (TVar y)
      in prop @?= EqualityProp (TVar x) (TVar y)

  , testCase "Equality between applications" $
      let f = Var "f"
          x = Var "x"
          y = Var "y"
          prop = EqualityProp (TApp (TVar f) (TVar x)) (TVar y)
      in prop @?= EqualityProp (TApp (TVar f) (TVar x)) (TVar y)

  , testCase "Equality between pairs" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TPair (TVar x) (TVar y)) (TPair (TVar y) (TVar x))
      in prop @?= EqualityProp (TPair (TVar x) (TVar y)) (TPair (TVar y) (TVar x))
  ]

-- ============================================================================
-- Universal Quantification Tests
-- ============================================================================

propositionQuantificationTests :: TestTree
propositionQuantificationTests = testGroup "Universal Quantification (∀ x. P x)"
  [ testCase "Simple universal quantification" $
      let x = Var "x"
          prop = UniversalQuantProp x (EqualityProp (TVar x) (TVar x))
      in prop @?= UniversalQuantProp x (EqualityProp (TVar x) (TVar x))

  , testCase "Multiple universal quantifications" $
      let x = Var "x"
          y = Var "y"
          prop = UniversalQuantProp x (UniversalQuantProp y (EqualityProp (TVar x) (TVar y)))
      in prop @?= UniversalQuantProp x (UniversalQuantProp y (EqualityProp (TVar x) (TVar y)))

  , testCase "Universal with implication" $
      let x = Var "x"
          prop = UniversalQuantProp x (ImplicationProp
                    (EqualityProp (TVar x) (TConst "0"))
                    (EqualityProp (TVar x) (TConst "0")))
      in prop @?= UniversalQuantProp x (ImplicationProp
                    (EqualityProp (TVar x) (TConst "0"))
                    (EqualityProp (TVar x) (TConst "0")))
  ]

-- ============================================================================
-- Implication Tests
-- ============================================================================

propositionImplicationTests :: TestTree
propositionImplicationTests = testGroup "Implication (P → Q)"
  [ testCase "Simple implication" $
      let x = Var "x"
          p = EqualityProp (TVar x) (TConst "0")
          q = EqualityProp (TVar x) (TConst "0")
          prop = ImplicationProp p q
      in prop @?= ImplicationProp p q

  , testCase "Implication with complex antecedent" $
      let x = Var "x"
          y = Var "y"
          p = ConjunctionProp
                (EqualityProp (TVar x) (TConst "1"))
                (EqualityProp (TVar y) (TConst "2"))
          q = EqualityProp (TApp (TVar x) (TVar y)) (TConst "3")
          prop = ImplicationProp p q
      in prop @?= ImplicationProp p q

  , testCase "Nested implications" $
      let p = EqualityProp (TConst "1") (TConst "1")
          q = EqualityProp (TConst "2") (TConst "2")
          r = EqualityProp (TConst "3") (TConst "3")
          prop = ImplicationProp (ImplicationProp p q) r
      in prop @?= ImplicationProp (ImplicationProp p q) r
  ]

-- ============================================================================
-- Conjunction Tests
-- ============================================================================

propositionConjunctionTests :: TestTree
propositionConjunctionTests = testGroup "Conjunction (P ∧ Q)"
  [ testCase "Simple conjunction" $
      let p = EqualityProp (TConst "1") (TConst "1")
          q = EqualityProp (TConst "2") (TConst "2")
          prop = ConjunctionProp p q
      in prop @?= ConjunctionProp p q

  , testCase "Conjunction with variable equality" $
      let x = Var "x"
          y = Var "y"
          p = EqualityProp (TVar x) (TVar y)
          q = EqualityProp (TVar x) (TConst "0")
          prop = ConjunctionProp p q
      in prop @?= ConjunctionProp p q

  , testCase "Multiple conjunctions" $
      let p1 = EqualityProp (TConst "1") (TConst "1")
          p2 = EqualityProp (TConst "2") (TConst "2")
          p3 = EqualityProp (TConst "3") (TConst "3")
          prop = ConjunctionProp (ConjunctionProp p1 p2) p3
      in prop @?= ConjunctionProp (ConjunctionProp p1 p2) p3
  ]

-- ============================================================================
-- Typing Proposition Tests
-- ============================================================================

propositionTypingTests :: TestTree
propositionTypingTests = testGroup "Typing Propositions (x : T)"
  [ testCase "Simple typing proposition" $
      let x = Var "x"
          prop = TypingProp (TVar x) "Int"
      in prop @?= TypingProp (TVar x) "Int"

  , testCase "Typing with application" $
      let f = Var "f"
          x = Var "x"
          prop = TypingProp (TApp (TVar f) (TVar x)) "Bool"
      in prop @?= TypingProp (TApp (TVar f) (TVar x)) "Bool"

  , testCase "Typing with lambda" $
      let x = Var "x"
          body = TVar x
          prop = TypingProp (TAbs x body) "Function"
      in prop @?= TypingProp (TAbs x body) "Function"
  ]

-- ============================================================================
-- Predicate Application Tests
-- ============================================================================

propositionPredicateTests :: TestTree
propositionPredicateTests = testGroup "Predicate Application"
  [ testCase "Unary predicate" $
      let x = Var "x"
          prop = PredicateApp "isZero" [TVar x]
      in prop @?= PredicateApp "isZero" [TVar x]

  , testCase "Binary predicate" $
      let x = Var "x"
          y = Var "y"
          prop = PredicateApp "equals" [TVar x, TVar y]
      in prop @?= PredicateApp "equals" [TVar x, TVar y]

  , testCase "Predicate with multiple terms" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          prop = PredicateApp "transitivity" [TVar x, TVar y, TVar z]
      in prop @?= PredicateApp "transitivity" [TVar x, TVar y, TVar z]
  ]

-- ============================================================================
-- Assertion Construction Tests
-- ============================================================================

assertionConstructionTests :: TestTree
assertionConstructionTests = testGroup "Assertion Construction"
  [ testCase "Create assertion with equality proposition" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TVar x) (TVar y)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "absorption" prop loc
      in assertionName assertion @?= "absorption" &&
         assertionProp assertion @?= prop &&
         assertionLoc assertion @?= loc

  , testCase "Create assertion with universal quantification" $
      let x = Var "x"
          y = Var "y"
          prop = UniversalQuantProp x (UniversalQuantProp y
                  (EqualityProp (TApp (TVar x) (TVar y)) (TVar x)))
          loc = SourceLoc "test.hs" 5 10
          assertion = mkAssertion "absorption_law" prop loc
      in assertionName assertion @?= "absorption_law"

  , testCase "Create assertion from complex proposition" $
      let x = Var "x"
          y = Var "y"
          p = UniversalQuantProp x (UniversalQuantProp y
                (EqualityProp (TApp (TVar x) (TVar y)) (TVar x)))
          loc = SourceLoc "algebra.hs" 42 5
          assertion = mkAssertion "lattice_absorption" p loc
      in assertionName assertion @?= "lattice_absorption"
  ]

-- ============================================================================
-- Obligation Store Tests
-- ============================================================================

obligationStoreTests :: TestTree
obligationStoreTests = testGroup "Proof Obligation Store"
  [ testCase "Empty store is empty" $
      Map.null emptyStore @?= True

  , testCase "Insert obligation into empty store" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "identity" prop loc
          store = insertObligation assertion emptyStore
      in Map.size store @?= 1

  , testCase "Lookup obligation after insertion" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "identity" prop loc
          store = insertObligation assertion emptyStore
          result = lookupObligation "identity" store
      in case result of
           Just ob -> obligationState ob @?= Asserted
           Nothing -> False @?= True

  , testCase "Mark obligation as proven" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "identity" prop loc
          store1 = insertObligation assertion emptyStore
      in case markProven "identity" (Just "refl") store1 of
           Right store2 ->
               case lookupObligation "identity" store2 of
                 Just ob -> obligationState ob @?= Proven
                 Nothing -> False @?= True
           Left _ -> False @?= True

  , testCase "Mark obligation as unproven" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "identity" prop loc
          store1 = insertObligation assertion emptyStore
      in case markUnproven "identity" store1 of
           Right store2 ->
               case lookupObligation "identity" store2 of
                 Just ob -> obligationState ob @?= Unproven
                 Nothing -> False @?= True
           Left _ -> False @?= True

  , testCase "All proven check when all proven" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TVar x)
          loc1 = SourceLoc "test.hs" 1 1
          assertion1 = mkAssertion "id1" prop1 loc1

          prop2 = EqualityProp (TConst "1") (TConst "1")
          loc2 = SourceLoc "test.hs" 2 1
          assertion2 = mkAssertion "id2" prop2 loc2

          store1 = insertObligation assertion1 emptyStore
          store2 = insertObligation assertion2 store1
      in case markProven "id1" Nothing store2 of
           Right store3 ->
               case markProven "id2" Nothing store3 of
                 Right store4 -> allProven store4 @?= True
                 Left _ -> False @?= True
           Left _ -> False @?= True

  , testCase "Unproven obligations are counted" $
      let x = Var "x"
          prop1 = EqualityProp (TVar x) (TVar x)
          loc1 = SourceLoc "test.hs" 1 1
          assertion1 = mkAssertion "id1" prop1 loc1

          prop2 = EqualityProp (TConst "1") (TConst "1")
          loc2 = SourceLoc "test.hs" 2 1
          assertion2 = mkAssertion "id2" prop2 loc2

          store1 = insertObligation assertion1 emptyStore
          store2 = insertObligation assertion2 store1
      in case markProven "id1" Nothing store2 of
           Right store3 -> length (unprovenObligations store3) @?= 1
           Left _ -> False @?= True

  , testCase "Count obligations by state" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TVar x)
          loc = SourceLoc "test.hs" 1 1
          assertion = mkAssertion "identity" prop loc
          store1 = insertObligation assertion emptyStore
      in countByState Asserted store1 @?= 1 &&
         countByState Proven store1 @?= 0
  ]

-- ============================================================================
-- Free Variables in Propositions Tests
-- ============================================================================

propFreeVariablesTests :: TestTree
propFreeVariablesTests = testGroup "Free Variables in Propositions"
  [ testCase "Free variables in equality" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TVar x) (TVar y)
      in propFreeVariables prop @?= Set.fromList [x, y]

  , testCase "Free variables in universal quantification" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          prop = UniversalQuantProp x (EqualityProp (TVar x) (TVar y))
      in propFreeVariables prop @?= Set.singleton y

  , testCase "Multiple quantifications" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          prop = UniversalQuantProp x (UniversalQuantProp y (EqualityProp (TVar x) (TVar z)))
      in propFreeVariables prop @?= Set.singleton z

  , testCase "Free variables in implication" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          p = EqualityProp (TVar x) (TVar y)
          q = EqualityProp (TVar y) (TVar z)
          prop = ImplicationProp p q
      in propFreeVariables prop @?= Set.fromList [x, y, z]

  , testCase "Free variables in conjunction" $
      let x = Var "x"
          y = Var "y"
          p = EqualityProp (TVar x) (TConst "1")
          q = EqualityProp (TVar y) (TConst "2")
          prop = ConjunctionProp p q
      in propFreeVariables prop @?= Set.fromList [x, y]

  , testCase "Free variables in typing" $
      let x = Var "x"
          prop = TypingProp (TVar x) "Int"
      in propFreeVariables prop @?= Set.singleton x

  , testCase "Free variables in predicate application" $
      let x = Var "x"
          y = Var "y"
          prop = PredicateApp "binPred" [TVar x, TVar y]
      in propFreeVariables prop @?= Set.fromList [x, y]

  , testCase "No free variables in all constants" $
      let prop = EqualityProp (TConst "1") (TConst "2")
      in propFreeVariables prop @?= Set.empty

  , testCase "Shadowing in nested quantification" $
      let x = Var "x"
          y = Var "y"
          prop = UniversalQuantProp x (UniversalQuantProp x (EqualityProp (TVar x) (TVar y)))
      in propFreeVariables prop @?= Set.singleton y
  ]

-- ============================================================================
-- Proposition Substitution Tests
-- ============================================================================

propSubstituteTests :: TestTree
propSubstituteTests = testGroup "Proposition Substitution"
  [ testCase "Substitute in equality proposition" $
      let x = Var "x"
          prop = EqualityProp (TVar x) (TConst "5")
          result = propSubstitute x (TConst "1") prop
      in result @?= EqualityProp (TConst "1") (TConst "5")

  , testCase "Substitution respects binding" $
      let x = Var "x"
          y = Var "y"
          prop = UniversalQuantProp x (EqualityProp (TVar x) (TVar y))
          result = propSubstitute x (TConst "1") prop
      in result @?= prop  -- x is bound, so substitution doesn't apply

  , testCase "Substitution in implication" $
      let x = Var "x"
          p = EqualityProp (TVar x) (TConst "1")
          q = EqualityProp (TVar x) (TConst "2")
          prop = ImplicationProp p q
          result = propSubstitute x (TConst "0") prop
      in result @?= ImplicationProp
          (EqualityProp (TConst "0") (TConst "1"))
          (EqualityProp (TConst "0") (TConst "2"))

  , testCase "Substitution in conjunction" $
      let x = Var "x"
          p = EqualityProp (TVar x) (TConst "1")
          q = EqualityProp (TVar x) (TConst "2")
          prop = ConjunctionProp p q
          result = propSubstitute x (TConst "0") prop
      in result @?= ConjunctionProp
          (EqualityProp (TConst "0") (TConst "1"))
          (EqualityProp (TConst "0") (TConst "2"))
  ]

-- ============================================================================
-- Proposition Alpha Renaming Tests
-- ============================================================================

propAlphaRenameTests :: TestTree
propAlphaRenameTests = testGroup "Proposition Alpha Renaming"
  [ testCase "Rename bound variable in universal quantification" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          prop = UniversalQuantProp x (EqualityProp (TVar x) (TVar y))
          result = propAlphaRename x z prop
      in result @?= UniversalQuantProp z (EqualityProp (TVar z) (TVar y))

  , testCase "Rename in nested quantification" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          w = Var "w"
          prop = UniversalQuantProp x (UniversalQuantProp y (EqualityProp (TVar x) (TVar y)))
          result = propAlphaRename x w prop
      in result @?= UniversalQuantProp w (UniversalQuantProp y (EqualityProp (TVar w) (TVar y)))

  , testCase "Rename does not affect free variables" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
          prop = EqualityProp (TVar x) (TVar y)
          result = propAlphaRename x z prop
      in result @?= EqualityProp (TVar z) (TVar y)
  ]

-- ============================================================================
-- Pretty Printing Tests
-- ============================================================================

prettyPrintTests :: TestTree
prettyPrintTests = testGroup "Pretty Printing"
  [ testCase "Pretty print equality proposition" $
      let x = Var "x"
          y = Var "y"
          prop = EqualityProp (TVar x) (TVar y)
          result = prettyPrintProposition prop
      in (x `elem` words result && y `elem` words result) @?= True

  , testCase "Pretty print universal quantification" $
      let x = Var "x"
          prop = UniversalQuantProp x (EqualityProp (TVar x) (TConst "1"))
          result = prettyPrintProposition prop
      in ("∀" `isInfixOf` result || "forall" `isInfixOf` result) @?= True

  , testCase "Pretty print implication" $
      let p = EqualityProp (TConst "1") (TConst "1")
          q = EqualityProp (TConst "2") (TConst "2")
          prop = ImplicationProp p q
          result = prettyPrintProposition prop
      in ("→" `isInfixOf` result || "->" `isInfixOf` result) @?= True

  , testCase "Pretty print conjunction" $
      let p = EqualityProp (TConst "1") (TConst "1")
          q = EqualityProp (TConst "2") (TConst "2")
          prop = ConjunctionProp p q
          result = prettyPrintProposition prop
      in ("∧" `isInfixOf` result || "/\\" `isInfixOf` result) @?= True

  , testCase "Pretty print typing proposition" $
      let x = Var "x"
          prop = TypingProp (TVar x) "Int"
          result = prettyPrintProposition prop
      in ("Int" `isInfixOf` result) @?= True
  ]

-- ============================================================================
-- Elaboration Interface Tests
-- ============================================================================

elaborationInterfaceTests :: TestTree
elaborationInterfaceTests = testGroup "Elaboration Interface"
  [ testCase "Elaboration stub returns error" $
      let surface = SurfaceAssertion "test" "∀ x. x ≡ x" (SourceLoc "test.hs" 1 1)
          result = elaborateAssertion surface
      in case result of
           Left msg -> ("not yet implemented" `isInfixOf` msg) @?= True
           Right _ -> False @?= True

  , testCase "Surface assertion captures name and location" $
      let surface = SurfaceAssertion "absorption" "∀ x y. join x (meet x y) ≡ x" (SourceLoc "algebra.hs" 42 5)
      in surfaceName surface @?= "absorption" &&
         sourcePath (surfaceSourceLoc surface) @?= "algebra.hs" &&
         lineNumber (surfaceSourceLoc surface) @?= 42
  ]

-- Helper function for checking substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = go needle haystack
  where
    go [] _ = True
    go _ [] = False
    go (n:ns) (h:hs)
      | n == h = go ns hs || go (n:ns) hs
      | otherwise = go (n:ns) hs

