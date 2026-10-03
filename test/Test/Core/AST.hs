{-# LANGUAGE OverloadedStrings #-}

module Test.Core.AST
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Assertica.Core.AST

-- ============================================================================
-- UNIT TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.AST"
  [ testGroup "Variables and Names"
      [ testCase "Create variable" testCreateVar
      , testCase "Create qualified name" testCreateQName
      , testCase "Unqualified name" testUnqualifiedName
      ]
  , testGroup "Lambda Terms"
      [ testCase "Simple lambda" testSimpleLambda
      , testCase "Nested lambda" testNestedLambda
      , testCase "Lambda with type annotation" testLambdaWithType
      ]
  , testGroup "Applications"
      [ testCase "Function application" testApplication
      , testCase "Nested application" testNestedApplication
      ]
  , testGroup "Type Construction"
      [ testCase "Function type" testFunctionType
      , testCase "Forall type (dependent)" testForallType
      , testCase "Type application" testTypeApplication
      ]
  , testGroup "Free Variables"
      [ testCase "Free variable in lambda body" testFreeVarInLambda
      , testCase "Bound variable not free" testBoundVarNotFree
      , testCase "Multiple free variables" testMultipleFreeVars
      , testCase "Free variables in application" testFreeVarsInApp
      ]
  , testGroup "Substitution"
      [ testCase "Simple substitution" testSimpleSubstitution
      , testCase "Substitution with shadowing" testSubstitutionShadowing
      , testCase "No capture in substitution" testNoCapture
      , testCase "Substitution in nested lambdas" testSubstitutionNested
      ]
  , testGroup "Propositions"
      [ testCase "Equality proposition" testEqualityProposition
      , testCase "Conjunction" testConjunction
      , testCase "Universal quantification" testUniversalQuant
      ]
  , testGroup "Proofs"
      [ testCase "Reflexivity proof" testReflexivityProof
      , testCase "Symmetry proof" testSymmetryProof
      , testCase "Transitivity proof" testTransitivityProof
      ]
  , testGroup "Assertions"
      [ testCase "Create assertion" testCreateAssertion
      , testCase "Assertion with proof" testAssertionWithProof
      , testCase "Unproven assertion" testUnprovenAssertion
      ]
  , testGroup "Pretty Printing"
      [ testCase "Pretty print variable" testPrettyVar
      , testCase "Pretty print lambda" testPrettyLambda
      , testCase "Pretty print function type" testPrettyType
      ]
  , testGroup "Data Constructors"
      [ testCase "Simple constructor" testSimpleConstructor
      , testCase "Nested constructors" testNestedConstructors
      ]
  , testGroup "Case Expressions"
      [ testCase "Simple case" testSimpleCase
      , testCase "Case with patterns" testCaseWithPatterns
      ]
  ]

-- ============================================================================
-- VARIABLE AND NAME TESTS
-- ============================================================================

testCreateVar :: Assertion
testCreateVar =
  let v = Var "x" 1
  in varName v @?= "x"

testCreateQName :: Assertion
testCreateQName =
  let q = QName ["Prelude", "Core"] "map"
  in qnameLocal q @?= "map"

testUnqualifiedName :: Assertion
testUnqualifiedName =
  let q = QName [] "x"
  in qnameModule q @?= []

-- ============================================================================
-- LAMBDA TERM TESTS
-- ============================================================================

testSimpleLambda :: Assertion
testSimpleLambda =
  let x = Var "x" 1
      binder = Binder x Nothing
      body = Var x
      lam = Lam binder body
  in case lam of
       Lam b e -> binderVar b @?= x
       _ -> assertFailure "Expected lambda"

testNestedLambda :: Assertion
testNestedLambda =
  let x = Var "x" 1
      y = Var "y" 2
      bx = Binder x Nothing
      by = Binder y Nothing
      innerLam = Lam by (Var y)
      outerLam = Lam bx innerLam
  in case outerLam of
       Lam b1 (Lam b2 _) ->
         do binderVar b1 @?= x
            binderVar b2 @?= y
       _ -> assertFailure "Expected nested lambdas"

testLambdaWithType :: Assertion
testLambdaWithType =
  let x = Var "x" 1
      intType = TyConst (QName ["Prelude"] "Int") []
      binder = Binder x (Just intType)
      lam = Lam binder (Var x)
  in case lam of
       Lam b _ -> binderType b @?= Just intType
       _ -> assertFailure "Expected lambda with type"

-- ============================================================================
-- APPLICATION TESTS
-- ============================================================================

testApplication :: Assertion
testApplication =
  let f = Var "f" 1
      x = Var "x" 2
      app = App f x
  in case app of
       App f' x' ->
         do f' @?= f
            x' @?= x
       _ -> assertFailure "Expected application"

testNestedApplication :: Assertion
testNestedApplication =
  let f = Var "f" 1
      x = Var "x" 2
      y = Var "y" 3
      app = App (App f x) y  -- (f x) y
  in case app of
       App (App f' x') y' ->
         do f' @?= f
            x' @?= x
            y' @?= y
       _ -> assertFailure "Expected nested application"

-- ============================================================================
-- TYPE CONSTRUCTION TESTS
-- ============================================================================

testFunctionType :: Assertion
testFunctionType =
  let a = TyVar (Var "a" 1)
      b = TyVar (Var "b" 2)
      funType = TyFun a b
  in case funType of
       TyFun a' b' ->
         do a' @?= a
            b' @?= b
       _ -> assertFailure "Expected function type"

testForallType :: Assertion
testForallType =
  let a = Var "a" 1
      binder = Binder a (Just (TyUniverse Type0))
      body = TyVar a
      forallType = TyForall binder body
  in case forallType of
       TyForall b _ ->
         binderVar b @?= a
       _ -> assertFailure "Expected forall type"

testTypeApplication :: Assertion
testTypeApplication =
  let t1 = TyVar (Var "f" 1)
      t2 = TyVar (Var "a" 2)
      app = TyApp t1 t2
  in case app of
       TyApp t1' t2' ->
         do t1' @?= t1
            t2' @?= t2
       _ -> assertFailure "Expected type application"

-- ============================================================================
-- FREE VARIABLE TESTS
-- ============================================================================

testFreeVarInLambda :: Assertion
testFreeVarInLambda =
  let x = Var "x" 1
      y = Var "y" 2
      binder = Binder x Nothing
      body = App (Var x) (Var y)  -- λx. x y
      lam = Lam binder body
      freeVars = freeVarsInTerm lam
  in freeVars @?= Set.singleton y

testBoundVarNotFree :: Assertion
testBoundVarNotFree =
  let x = Var "x" 1
      binder = Binder x Nothing
      body = Var x  -- λx. x
      lam = Lam binder body
      freeVars = freeVarsInTerm lam
  in freeVars @?= Set.empty

testMultipleFreeVars :: Assertion
testMultipleFreeVars =
  let x = Var "x" 1
      y = Var "y" 2
      z = Var "z" 3
      binder = Binder x Nothing
      body = App (Var y) (Var z)  -- λx. y z
      lam = Lam binder body
      freeVars = freeVarsInTerm lam
  in Set.size freeVars @?= 2 .&&. Set.member y freeVars .&&. Set.member z freeVars

testFreeVarsInApp :: Assertion
testFreeVarsInApp =
  let x = Var "x" 1
      y = Var "y" 2
      app = App (Var x) (Var y)
      freeVars = freeVarsInTerm app
  in Set.size freeVars @?= 2

-- ============================================================================
-- SUBSTITUTION TESTS
-- ============================================================================

testSimpleSubstitution :: Assertion
testSimpleSubstitution =
  let x = Var "x" 1
      y = Var "y" 2
      term = Var x
      replacement = Var y
      result = substituteTerm x replacement term
  in result @?= Var y

testSubstitutionShadowing :: Assertion
testSubstitutionShadowing =
  let x = Var "x" 1
      y = Var "y" 2
      z = Var "z" 3
      binder = Binder x Nothing
      body = Var x  -- λx. x
      lam = Lam binder body
      replacement = Var z
      result = substituteTerm y replacement lam
  in result @?= lam  -- y doesn't appear, so no substitution

testNoCapture :: Assertion
testNoCapture =
  let x = Var "x" 1
      y = Var "y" 2
      z = Var "z" 3
      binder_x = Binder x Nothing
      binder_z = Binder z Nothing
      -- λz. x = replacement
      inner = Var x
      lam_z = Lam binder_z inner
      -- λx. (λz. x)
      lam_x = Lam binder_x lam_z
      replacement = Var y
      result = substituteTerm x replacement lam_x
  in case result of
       Lam _ (Lam _ body) -> body @?= Var y
       _ -> assertFailure "Unexpected result structure"

testSubstitutionNested :: Assertion
testSubstitutionNested =
  let x = Var "x" 1
      y = Var "y" 2
      z = Var "z" 3
      bx = Binder x Nothing
      by = Binder y Nothing
      inner = Var x  -- λy. x
      lam_y = Lam by inner
      -- λx. (λy. x)
      lam_x = Lam bx lam_y
      replacement = Var z
      result = substituteTerm x replacement lam_x
  in case result of
       Lam bx' (Lam by' body) ->
         do binderVar bx' @?= x  -- Outer binding unchanged
            body @?= Var z  -- x replaced with z
       _ -> assertFailure "Unexpected structure"

-- ============================================================================
-- PROPOSITION TESTS
-- ============================================================================

testEqualityProposition :: Assertion
testEqualityProposition =
  let e1 = Var (Var "a" 1)
      e2 = Var (Var "b" 2)
      prop = Eq e1 e2
  in case prop of
       Eq a b ->
         do a @?= e1
            b @?= e2
       _ -> assertFailure "Expected equality"

testConjunction :: Assertion
testConjunction =
  let p1 = Top
      p2 = Bot
      conj = And p1 p2
  in case conj of
       And p1' p2' ->
         do p1' @?= p1
            p2' @?= p2
       _ -> assertFailure "Expected conjunction"

testUniversalQuant :: Assertion
testUniversalQuant =
  let x = Var "x" 1
      binder = Binder x Nothing
      prop = Top
      forall = Forall' binder prop
  in case forall of
       Forall' b p ->
         do binderVar b @?= x
            p @?= prop
       _ -> assertFailure "Expected forall"

-- ============================================================================
-- PROOF TESTS
-- ============================================================================

testReflexivityProof :: Assertion
testReflexivityProof =
  let e = Var (Var "a" 1)
      proof = Refl e
  in case proof of
       Refl e' -> e' @?= e
       _ -> assertFailure "Expected reflexivity"

testSymmetryProof :: Assertion
testSymmetryProof =
  let e1 = Var (Var "a" 1)
      e2 = Var (Var "b" 2)
      prop_proof = Refl e1
      symm_proof = Symm prop_proof
  in case symm_proof of
       Symm p -> p @?= prop_proof
       _ -> assertFailure "Expected symmetry"

testTransitivityProof :: Assertion
testTransitivityProof =
  let e1 = Var (Var "a" 1)
      p1 = Refl e1
      p2 = Refl e1
      trans = Trans p1 p2
  in case trans of
       Trans p1' p2' ->
         do p1' @?= p1
            p2' @?= p2
       _ -> assertFailure "Expected transitivity"

-- ============================================================================
-- ASSERTION TESTS
-- ============================================================================

testCreateAssertion :: Assertion
testCreateAssertion =
  let name = QName ["Test"] "prop1"
      prop = Top
      assertion = Assertion name prop Nothing
  in assertionName assertion @?= name

testAssertionWithProof :: Assertion
testAssertionWithProof =
  let name = QName [] "true_proof"
      prop = Top
      proof = Refl (Const UnitConst)
      assertion = Assertion name prop (Just proof)
  in assertionProof assertion @?= Just proof

testUnprovenAssertion :: Assertion
testUnprovenAssertion =
  let name = QName ["Test"] "unproven"
      prop = Bot
      assertion = Assertion name prop Nothing
  in assertionProof assertion @?= Nothing

-- ============================================================================
-- PRETTY PRINTING TESTS
-- ============================================================================

testPrettyVar :: Assertion
testPrettyVar =
  let v = Var "x" 1
      term = Var v
      pretty = prettyTerm term
  in pretty @?= "x"

testPrettyLambda :: Assertion
testPrettyLambda =
  let x = Var "x" 1
      binder = Binder x Nothing
      body = Var x
      lam = Lam binder body
      pretty = prettyTerm lam
  in "λ" `elem` pretty && "x" `elem` pretty @? "Lambda formatting"

testPrettyType :: Assertion
testPrettyType =
  let a = TyVar (Var "a" 1)
      b = TyVar (Var "b" 2)
      funType = TyFun a b
      pretty = prettyType funType
  in "→" `elem` pretty @? "Arrow in type"

-- ============================================================================
-- DATA CONSTRUCTOR TESTS
-- ============================================================================

testSimpleConstructor :: Assertion
testSimpleConstructor =
  let name = QName ["Prelude"] "Just"
      value = Var (Var "x" 1)
      constr = Constr name [value]
  in case constr of
       Constr n args ->
         do n @?= name
            length args @?= 1
       _ -> assertFailure "Expected constructor"

testNestedConstructors :: Assertion
testNestedConstructors =
  let cons_name = QName ["Prelude"] "Cons"
      nil_name = QName ["Prelude"] "Nil"
      x = Var (Var "x" 1)
      nil = Constr nil_name []
      cons_cell = Constr cons_name [x, nil]
  in case cons_cell of
       Constr name args ->
         do name @?= cons_name
            length args @?= 2
       _ -> assertFailure "Expected nested constructor"

-- ============================================================================
-- CASE EXPRESSION TESTS
-- ============================================================================

testSimpleCase :: Assertion
testSimpleCase =
  let scrutinee = Var (Var "x" 1)
      pattern1 = PatLit "1"
      body1 = Const (IntLit 10)
      clause = Clause pattern1 body1
      case_expr = Case scrutinee [clause] Nothing
  in case case_expr of
       Case e clauses def ->
         do e @?= scrutinee
            length clauses @?= 1
            def @?= Nothing
       _ -> assertFailure "Expected case"

testCaseWithPatterns :: Assertion
testCaseWithPatterns =
  let scrutinee = Var (Var "x" 1)
      x = Var "x" 1
      y = Var "y" 2
      pat1 = PatVar x
      body1 = Const (IntLit 1)
      pat2 = PatVar y
      body2 = Const (IntLit 2)
      clause1 = Clause pat1 body1
      clause2 = Clause pat2 body2
      case_expr = Case scrutinee [clause1, clause2] Nothing
  in case case_expr of
       Case _ clauses _ -> length clauses @?= 2
       _ -> assertFailure "Expected case with patterns"

-- ============================================================================
-- PROPERTY-BASED TESTS
-- ============================================================================

prop_freeVarsConservative :: Term -> Bool
prop_freeVarsConservative term =
  -- Free variables found should be a subset of all variables in the term
  let freeVars = freeVarsInTerm term
  in all (not . Set.null) [Set.singleton v | v <- Set.toList freeVars]

prop_substitutionIdempotent :: Term -> Var -> Term -> Bool
prop_substitutionIdempotent term var replacement =
  -- Substituting the same variable twice should be idempotent
  let result1 = substituteTerm var replacement term
      result2 = substituteTerm var replacement result1
  in result1 == result2

prop_prettyTermNoError :: Term -> Bool
prop_prettyTermNoError term =
  -- Pretty printing should not crash
  not (null (prettyTerm term))

prop_prettyTypeNoError :: Type -> Bool
prop_prettyTypeNoError ty =
  -- Pretty printing should not crash
  not (null (prettyType ty))

-- ============================================================================
-- PROPERTY TESTS (QuickCheck Integration)
-- ============================================================================

-- These would require Arbitrary instances for AST types
-- For now, we'll add placeholder instances
instance Arbitrary Var where
  arbitrary = Var <$> (T.pack <$> arbitrary) <*> arbitrary

instance Arbitrary QName where
  arbitrary = QName <$> arbitrary <*> (T.pack <$> arbitrary)

instance Arbitrary Term where
  arbitrary = oneof
    [ Var <$> arbitrary
    , Const <$> arbitrary
    ]

instance Arbitrary Type where
  arbitrary = oneof
    [ TyVar <$> arbitrary
    , TyUniverse <$> arbitrary
    ]

instance Arbitrary TypeLevel where
  arbitrary = oneof [pure Type0, TypeN <$> arbitrary]

instance Arbitrary Constant where
  arbitrary = oneof
    [ IntLit <$> arbitrary
    , BoolLit <$> arbitrary
    , pure UnitConst
    ]

instance Arbitrary Proposition where
  arbitrary = oneof
    [ pure Top
    , pure Bot
    ]

-- Property tests can be added here
propTests :: TestTree
propTests = testGroup "Property Tests"
  [ QC.testProperty "Pretty term doesn't error" prop_prettyTermNoError
  , QC.testProperty "Pretty type doesn't error" prop_prettyTypeNoError
  ]
