{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.TypeCompiler
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Set as Set

import Assertica.Core.AST
import Assertica.Backend.CodeGen (defaultConfig)
import Assertica.Backend.TypeCompiler

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.TypeCompiler"
  [ testGroup "Type Variables"
      [ testCase "Compile type variable" testTypeVariable
      ]
  , testGroup "Type Constants"
      [ testCase "Unqualified type constructor" testUnqualifiedType
      , testCase "Qualified type constructor" testQualifiedType
      , testCase "Type constructor with arguments" testTypeWithArgs
      ]
  , testGroup "Function Types"
      [ testCase "Simple function type" testSimpleFunctionType
      , testCase "Nested function type" testNestedFunctionType
      , testCase "Function type with multiple args" testMultiArgFunctionType
      ]
  , testGroup "Universe Types"
      [ testCase "Type0 universe" testType0Universe
      , testCase "TypeN universe" testTypeNUniverse
      ]
  , testGroup "Dependent Types"
      [ testCase "Forall type" testForallType
      ]
  , testGroup "Type Application"
      [ testCase "Type application" testTypeApplication
      ]
  , testGroup "Equality Types"
      [ testCase "Equality type compilation" testEqualityType
      ]
  , testGroup "Determinism"
      [ testCase "Type compilation is deterministic" testTypeDeterminism
      ]
  ]

-- ============================================================================
-- UNIT TESTS: TYPE VARIABLES
-- ============================================================================

testTypeVariable :: Assertion
testTypeVariable = do
  let tv = TyVar (Var "a" 0)
  result <- case compileType defaultConfig tv of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be a" "a" result

-- ============================================================================
-- UNIT TESTS: TYPE CONSTANTS
-- ============================================================================

testUnqualifiedType :: Assertion
testUnqualifiedType = do
  let ty = TyConst (QName [] "Int") []
  result <- case compileType defaultConfig ty of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be Int" "Int" result

testQualifiedType :: Assertion
testQualifiedType = do
  let ty = TyConst (QName ["Data", "List"] "List") []
  result <- case compileType defaultConfig ty of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain List" ("List" `T.isInfixOf` result)
  assertBool "Should contain qualified name" ("Data" `T.isInfixOf` result)

testTypeWithArgs :: Assertion
testTypeWithArgs = do
  let elemType = TyConst (QName [] "Int") []
  let listType = TyConst (QName [] "List") [elemType]
  result <- case compileType defaultConfig listType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain List" ("List" `T.isInfixOf` result)
  assertBool "Should contain Int" ("Int" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: FUNCTION TYPES
-- ============================================================================

testSimpleFunctionType :: Assertion
testSimpleFunctionType = do
  let intType = TyConst (QName [] "Int") []
  let boolType = TyConst (QName [] "Bool") []
  let funType = TyFun intType boolType
  result <- case compileType defaultConfig funType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain arrow" ("->" `T.isInfixOf` result)

testNestedFunctionType :: Assertion
testNestedFunctionType = do
  let intType = TyConst (QName [] "Int") []
  let boolType = TyConst (QName [] "Bool") []
  let charType = TyConst (QName [] "Char") []

  -- Int -> (Bool -> Char)
  let inner = TyFun boolType charType
  let outer = TyFun intType inner

  result <- case compileType defaultConfig outer of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err

  let hasArrows = (T.count "->" result) >= 2
  assertBool "Should contain multiple arrows" hasArrows

testMultiArgFunctionType :: Assertion
testMultiArgFunctionType = do
  let intType = TyConst (QName [] "Int") []
  let stringType = TyConst (QName [] "String") []
  let boolType = TyConst (QName [] "Bool") []

  -- Int -> String -> Bool
  let t1 = TyFun intType stringType
  let t2 = TyFun t1 boolType

  result <- case compileType defaultConfig t2 of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err

  assertBool "Should contain Int" ("Int" `T.isInfixOf` result)
  assertBool "Should contain String" ("String" `T.isInfixOf` result)
  assertBool "Should contain Bool" ("Bool" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: UNIVERSE TYPES
-- ============================================================================

testType0Universe :: Assertion
testType0Universe = do
  let univType = TyUniverse Type0
  result <- case compileType defaultConfig univType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be Type" "Type" result

testTypeNUniverse :: Assertion
testTypeNUniverse = do
  let univType = TyUniverse (TypeN 5)
  result <- case compileType defaultConfig univType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  -- Higher universes map to Type
  assertEqual "Should be Type" "Type" result

-- ============================================================================
-- UNIT TESTS: DEPENDENT TYPES
-- ============================================================================

testForallType :: Assertion
testForallType = do
  let varType = TyVar (Var "a" 0)
  let bodyType = TyVar (Var "a" 0)  -- forall a . a (identity)
  let forallType = TyForall (Binder (Var "a" 0) Nothing) bodyType

  result <- case compileType defaultConfig forallType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err

  assertBool "Should contain forall" ("forall" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: TYPE APPLICATION
-- ============================================================================

testTypeApplication :: Assertion
testTypeApplication = do
  let listConstr = TyConst (QName [] "List") []
  let intType = TyConst (QName [] "Int") []
  let appType = TyApp listConstr intType

  result <- case compileType defaultConfig appType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err

  assertBool "Should contain List" ("List" `T.isInfixOf` result)
  assertBool "Should contain Int" ("Int" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: EQUALITY TYPES
-- ============================================================================

testEqualityType :: Assertion
testEqualityType = do
  let x = Var (Var "x" 0)
  let y = Var (Var "y" 0)
  let eqType = TyEq (Var x) (Var y)

  result <- case compileType defaultConfig eqType of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err

  -- Equality type should compile to something
  assertBool "Should produce valid output" (not (T.null result))

-- ============================================================================
-- PROPERTY TESTS: DETERMINISM
-- ============================================================================

testTypeDeterminism :: Assertion
testTypeDeterminism = do
  let intType = TyConst (QName [] "Int") []
  let boolType = TyConst (QName [] "Bool") []
  let funType = TyFun intType boolType

  let result1 = compileType defaultConfig funType
  let result2 = compileType defaultConfig funType

  assertEqual "Type compilation should be deterministic" result1 result2
