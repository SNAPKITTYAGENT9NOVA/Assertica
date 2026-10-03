{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Core.TypeChecker
Description : Tests for the type checker (Agent 3B)
-}

module Test.Core.TypeChecker
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Core.TypeChecker
import Assertica.Core.TypeEnv

-- ============================================================================
-- UNIT TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.TypeChecker"
  [ testGroup "Type Environment"
      [ testCase "Create empty environment" testEmptyEnv
      , testCase "Extend environment with variable" testExtendEnv
      , testCase "Lookup variable in environment" testLookupVar
      , testCase "Extend with definition" testExtendWithDef
      , testCase "Pretty print environment" testPrettyEnv
      ]

  , testGroup "Basic Type Checking"
      [ testCase "Type of integer constant" testTypeOfInt
      , testCase "Type of boolean constant" testTypeOfBool
      , testCase "Type of string constant" testTypeOfString
      , testCase "Type of unit constant" testTypeOfUnit
      ]

  , testGroup "Variable Typing"
      [ testCase "Type of variable in scope" testTypeOfVarInScope
      , testCase "Undefined variable error" testUndefinedVarError
      ]

  , testGroup "Lambda Abstractions"
      [ testCase "Simple lambda type" testSimpleLambdaType
      , testCase "Nested lambda type" testNestedLambdaType
      , testCase "Lambda without annotation fails" testLambdaNoAnnotationFails
      ]

  , testGroup "Function Application"
      [ testCase "Application of simple function" testSimpleApplication
      , testCase "Nested application" testNestedApplication
      , testCase "Application type mismatch" testApplicationTypeMismatch
      , testCase "Application of non-function fails" testApplicationNonFunctionFails
      ]

  , testGroup "Dependent Types"
      [ testCase "Dependent function type" testDependentFunctionType
      , testCase "Dependent function application" testDependentApplicationType
      ]

  , testGroup "Let Bindings"
      [ testCase "Simple let binding" testSimpleLetBinding
      , testCase "Let binding with type inference" testLetBindingTypeInference
      ]

  , testGroup "Type Annotations"
      [ testCase "Type annotation success" testAnnotationSuccess
      , testCase "Type annotation mismatch" testAnnotationMismatch
      ]

  , testGroup "Propositions"
      [ testCase "Equality proposition type" testEqualityPropositionType
      , testCase "Conjunction proposition type" testConjunctionPropositionType
      , testCase "Universal quantification type" testUniversalQuantType
      ]

  , testGroup "Forall Types"
      [ testCase "Forall type universe" testForallUniverse
      ]

  , testGroup "Error Messages"
      [ testCase "Pretty print type error" testPrettyTypeError
      ]
  ]

-- ============================================================================
-- TYPE ENVIRONMENT TESTS
-- ============================================================================

testEmptyEnv :: Assertion
testEmptyEnv =
  let env = emptyEnv
  in envSize env @?= 0

testExtendEnv :: Assertion
testExtendEnv =
  let env = emptyEnv
      v = Var "x" 1
      ty = TyConst (QName [] "Int") []
      result = extendEnv env v ty
  in case result of
    Right env' -> envSize env' @?= 1
    Left err -> assertFailure ("Failed to extend env: " ++ err)

testLookupVar :: Assertion
testLookupVar =
  let env = emptyEnv
      v = Var "x" 1
      intTy = TyConst (QName [] "Int") []
  in case extendEnv env v intTy of
    Right env' ->
      case lookupVar v env' of
        Just ty -> ty @?= intTy
        Nothing -> assertFailure "Lookup failed"
    Left err -> assertFailure ("Failed to extend env: " ++ err)

testExtendWithDef :: Assertion
testExtendWithDef =
  let env = emptyEnv
      f = QName [] "append"
      listATy = TyConst (QName [] "List") [TyVar (Var "a" 0)]
      funcTy = TyFun listATy (TyFun listATy listATy)
  in case extendWithDef env f funcTy of
    Right env' ->
      case lookupDef f env' of
        Just ty -> ty @?= funcTy
        Nothing -> assertFailure "Lookup definition failed"
    Left err -> assertFailure ("Failed to extend with def: " ++ err)

testPrettyEnv :: Assertion
testPrettyEnv =
  let env = emptyEnv
      v = Var "x" 1
      intTy = TyConst (QName [] "Int") []
  in case extendEnv env v intTy of
    Right env' ->
      let str = prettyEnv env'
      in assertBool "Environment pretty print" (not (null str))
    Left err -> assertFailure ("Failed to extend env: " ++ err)

-- ============================================================================
-- BASIC TYPE CHECKING TESTS
-- ============================================================================

testTypeOfInt :: Assertion
testTypeOfInt =
  let result = typeOfConst (IntLit 42)
      expected = Right (TyConst (QName [] "Int") [])
  in case result of
    Right ty -> ty @?= (TyConst (QName [] "Int") [])
    Left err -> assertFailure ("Failed to type int: " ++ prettyTypeError err)

testTypeOfBool :: Assertion
testTypeOfBool =
  let result = typeOfConst (BoolLit True)
      expected = Right (TyConst (QName [] "Bool") [])
  in case result of
    Right ty -> ty @?= (TyConst (QName [] "Bool") [])
    Left err -> assertFailure ("Failed to type bool: " ++ prettyTypeError err)

testTypeOfString :: Assertion
testTypeOfString =
  let result = typeOfConst (StringLit "hello")
      expected = Right (TyConst (QName [] "String") [])
  in case result of
    Right ty -> ty @?= (TyConst (QName [] "String") [])
    Left err -> assertFailure ("Failed to type string: " ++ prettyTypeError err)

testTypeOfUnit :: Assertion
testTypeOfUnit =
  let result = typeOfConst UnitConst
      expected = Right (TyConst (QName [] "Unit") [])
  in case result of
    Right ty -> ty @?= (TyConst (QName [] "Unit") [])
    Left err -> assertFailure ("Failed to type unit: " ++ prettyTypeError err)

-- ============================================================================
-- VARIABLE TYPING TESTS
-- ============================================================================

testTypeOfVarInScope :: Assertion
testTypeOfVarInScope =
  let env = emptyEnv
      v = Var "x" 1
      intTy = TyConst (QName [] "Int") []
  in case extendEnv env v intTy of
    Right env' ->
      case typeOfVar env' v of
        Right ty -> ty @?= intTy
        Left err -> assertFailure ("Failed to type var: " ++ prettyTypeError err)
    Left err -> assertFailure ("Failed to extend env: " ++ err)

testUndefinedVarError :: Assertion
testUndefinedVarError =
  let env = emptyEnv
      v = Var "undefined" 99
  in case typeOfVar env v of
    Right _ -> assertFailure "Should have failed for undefined variable"
    Left (UndefinedVariable _) -> return ()
    Left err -> assertFailure ("Wrong error type: " ++ prettyTypeError err)

-- ============================================================================
-- LAMBDA ABSTRACTION TESTS
-- ============================================================================

testSimpleLambdaType :: Assertion
testSimpleLambdaType =
  let env = emptyEnv
      v = Var "x" 1
      intTy = TyConst (QName [] "Int") []
      body = Var v  -- λx : Int. x
      binder = Binder v (Just intTy)
  in case typeOfLam env binder body of
    Right ty ->
      let expected = TyFun intTy intTy
      in ty @?= expected
    Left err -> assertFailure ("Failed to type lambda: " ++ prettyTypeError err)

testNestedLambdaType :: Assertion
testNestedLambdaType =
  let env = emptyEnv
      v1 = Var "x" 1
      v2 = Var "y" 2
      intTy = TyConst (QName [] "Int") []
      innerBody = Var v2
      innerBinder = Binder v2 (Just intTy)
      innerLam = Lam innerBinder innerBody
      outerBinder = Binder v1 (Just intTy)
  in case typeOfLam env outerBinder innerLam of
    Right ty ->
      let innerTy = TyFun intTy intTy
          expected = TyFun intTy innerTy
      in ty @?= expected
    Left err -> assertFailure ("Failed to type nested lambda: " ++ prettyTypeError err)

testLambdaNoAnnotationFails :: Assertion
testLambdaNoAnnotationFails =
  let env = emptyEnv
      v = Var "x" 1
      body = Var v
      binder = Binder v Nothing
  in case typeOfLam env binder body of
    Right _ -> assertFailure "Should have failed for lambda without annotation"
    Left _ -> return ()

-- ============================================================================
-- FUNCTION APPLICATION TESTS
-- ============================================================================

testSimpleApplication :: Assertion
testSimpleApplication =
  let env = emptyEnv
      -- λx : Int. x
      x = Var "x" 1
      intTy = TyConst (QName [] "Int") []
      identity = Lam (Binder x (Just intTy)) (Var x)
      -- apply to 5
      five = Const (IntLit 5)
  in case typeOfApp env identity five of
    Right ty -> ty @?= intTy
    Left err -> assertFailure ("Failed to type application: " ++ prettyTypeError err)

testNestedApplication :: Assertion
testNestedApplication =
  let env = emptyEnv
      intTy = TyConst (QName [] "Int") []
      -- ((λx : Int. λy : Int. x + y) 5) 3
      x = Var "x" 1
      y = Var "y" 2
      -- Inner lambda: λy. x + y (but x+y is tricky to type without Add)
      -- For now, just use y: λy : Int. y
      innerBody = Var y
      innerLam = Lam (Binder y (Just intTy)) innerBody
      -- Outer lambda: λx : Int. (λy. y)
      outerLam = Lam (Binder x (Just intTy)) innerLam
      -- Apply first to 5
      five = Const (IntLit 5)
  in case typeOfApp env outerLam five of
    Right ty ->
      let expected = TyFun intTy intTy
      in ty @?= expected
    Left err -> assertFailure ("Failed to type nested application: " ++ prettyTypeError err)

testApplicationTypeMismatch :: Assertion
testApplicationTypeMismatch =
  let env = emptyEnv
      intTy = TyConst (QName [] "Int") []
      boolTy = TyConst (QName [] "Bool") []
      -- λx : Int. x
      x = Var "x" 1
      idInt = Lam (Binder x (Just intTy)) (Var x)
      -- Apply to true
      trueVal = Const (BoolLit True)
  in case typeOfApp env idInt trueVal of
    Right _ -> assertFailure "Should have failed for type mismatch"
    Left (TypeMismatch {}) -> return ()
    Left err -> assertFailure ("Wrong error type: " ++ prettyTypeError err)

testApplicationNonFunctionFails :: Assertion
testApplicationNonFunctionFails =
  let env = emptyEnv
      five = Const (IntLit 5)
      three = Const (IntLit 3)
  in case typeOfApp env five three of
    Right _ -> assertFailure "Should have failed for application of non-function"
    Left (NotAFunction _) -> return ()
    Left err -> assertFailure ("Wrong error type: " ++ prettyTypeError err)

-- ============================================================================
-- DEPENDENT TYPE TESTS
-- ============================================================================

testDependentFunctionType :: Assertion
testDependentFunctionType =
  let env = emptyEnv
      a = Var "a" 0
      -- ∀ a : Type. a → a
      typeUniverse = TyUniverse Type0
      body = TyFun (TyVar a) (TyVar a)
      binder = Binder a (Just typeUniverse)
  in case typeOfForall env binder (Var a) of
    Right ty -> ty @?= (TyUniverse Type0)
    Left err -> assertFailure ("Failed to type forall: " ++ prettyTypeError err)

testDependentApplicationType :: Assertion
testDependentApplicationType =
  let env = emptyEnv
      a = Var "a" 0
      intTy = TyConst (QName [] "Int") []
      -- (λ a : Type. a) Int
      typeUniverse = TyUniverse Type0
      identityType = Lam (Binder a (Just typeUniverse)) (Var a)
  in case typeOfApp env identityType intTy of
    Right ty -> ty @?= intTy
    Left err -> assertFailure ("Failed dependent application: " ++ prettyTypeError err)

-- ============================================================================
-- LET BINDING TESTS
-- ============================================================================

testSimpleLetBinding :: Assertion
testSimpleLetBinding =
  let env = emptyEnv
      x = Var "x" 1
      intTy = TyConst (QName [] "Int") []
      five = Const (IntLit 5)
      body = Var x
      binder = Binder x Nothing
  in case typeOfLet env binder five body of
    Right ty -> ty @?= intTy
    Left err -> assertFailure ("Failed let binding: " ++ prettyTypeError err)

testLetBindingTypeInference :: Assertion
testLetBindingTypeInference =
  let env = emptyEnv
      x = Var "x" 1
      five = Const (IntLit 5)
      body = Var x
      binder = Binder x Nothing
  in case typeOfLet env binder five body of
    Right ty ->
      let expected = TyConst (QName [] "Int") []
      in ty @?= expected
    Left err -> assertFailure ("Failed let type inference: " ++ prettyTypeError err)

-- ============================================================================
-- TYPE ANNOTATION TESTS
-- ============================================================================

testAnnotationSuccess :: Assertion
testAnnotationSuccess =
  let env = emptyEnv
      five = Const (IntLit 5)
      intTy = TyConst (QName [] "Int") []
  in case typeOfAnn env five intTy of
    Right ty -> ty @?= intTy
    Left err -> assertFailure ("Failed type annotation: " ++ prettyTypeError err)

testAnnotationMismatch :: Assertion
testAnnotationMismatch =
  let env = emptyEnv
      five = Const (IntLit 5)
      boolTy = TyConst (QName [] "Bool") []
  in case typeOfAnn env five boolTy of
    Right _ -> assertFailure "Should have failed for annotation mismatch"
    Left _ -> return ()

-- ============================================================================
-- PROPOSITION TESTS
-- ============================================================================

testEqualityPropositionType :: Assertion
testEqualityPropositionType =
  let env = emptyEnv
      x = Const (IntLit 5)
      y = Const (IntLit 5)
      prop = Eq x y
  in case typeOfProposition env prop of
    Right ty -> ty @?= (TyConst (QName [] "Proposition") [])
    Left err -> assertFailure ("Failed proposition type: " ++ prettyTypeError err)

testConjunctionPropositionType :: Assertion
testConjunctionPropositionType =
  let env = emptyEnv
      x = Const (IntLit 5)
      y = Const (IntLit 5)
      prop = And (Eq x y) Top
  in case typeOfProposition env prop of
    Right ty -> ty @?= (TyConst (QName [] "Proposition") [])
    Left err -> assertFailure ("Failed conjunction type: " ++ prettyTypeError err)

testUniversalQuantType :: Assertion
testUniversalQuantType =
  let env = emptyEnv
      x = Var "x" 1
      intTy = TyConst (QName [] "Int") []
      prop = Forall' (Binder x (Just intTy)) (Eq (Var x) (Var x))
  in case typeOfProposition env prop of
    Right ty -> ty @?= (TyConst (QName [] "Proposition") [])
    Left err -> assertFailure ("Failed quantified proposition: " ++ prettyTypeError err)

-- ============================================================================
-- FORALL TYPE TESTS
-- ============================================================================

testForallUniverse :: Assertion
testForallUniverse =
  let env = emptyEnv
      a = Var "a" 0
      typeUniverse = TyUniverse Type0
      body = TyVar a
      binder = Binder a (Just typeUniverse)
  in case typeOfForall env binder (Const UnitConst) of
    Right ty ->
      case ty of
        TyUniverse _ -> return ()
        _ -> assertFailure ("Expected universe level, got: " ++ prettyType ty)
    Left err -> assertFailure ("Failed forall universe: " ++ prettyTypeError err)

-- ============================================================================
-- ERROR MESSAGE TESTS
-- ============================================================================

testPrettyTypeError :: Assertion
testPrettyTypeError =
  let err = UndefinedVariable (Var "x" 1)
      msg = prettyTypeError err
  in assertBool "Error message not empty" (not (null msg))
