{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.TermCompiler
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as Set

import Assertica.Core.AST
import Assertica.Backend.CodeGen (defaultConfig)
import Assertica.Backend.TermCompiler

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.TermCompiler"
  [ testGroup "Variable Compilation"
      [ testCase "Compile simple variable" testSimpleVariable
      , testCase "Variable name preserved" testVariableNamePreserved
      ]
  , testGroup "Constant Compilation"
      [ testCase "Compile integer constant" testIntConstant
      , testCase "Compile boolean constant" testBoolConstant
      , testCase "Compile unit constant" testUnitConst
      ]
  , testGroup "Lambda Abstraction"
      [ testCase "Compile simple lambda" testSimpleLambda
      , testCase "Lambda contains backslash" testLambdaBackslash
      , testCase "Nested lambda" testNestedLambda
      ]
  , testGroup "Function Application"
      [ testCase "Simple application" testSimpleApplication
      , testCase "Application with multiple args" testMultipleApplication
      ]
  , testGroup "Data Constructors"
      [ testCase "Simple constructor" testSimpleConstructor
      , testCase "Constructor with arguments" testConstructorWithArgs
      ]
  , testGroup "Case Expressions"
      [ testCase "Simple case" testSimpleCase
      , testCase "Case with default" testCaseWithDefault
      ]
  , testGroup "Let-Bindings"
      [ testCase "Simple let binding" testSimpleLet
      , testCase "Let with variable in body" testLetWithVar
      ]
  , testGroup "Type Annotation"
      [ testCase "Annotated term" testAnnotatedTerm
      ]
  , testGroup "Pattern Matching"
      [ testCase "Variable pattern" testPatternVar
      , testCase "Constructor pattern" testPatternConstructor
      , testCase "Wildcard pattern" testPatternWildcard
      ]
  , testGroup "Determinism"
      [ testCase "Compilation is deterministic" testDeterminism
      ]
  ]

-- ============================================================================
-- UNIT TESTS: VARIABLES
-- ============================================================================

testSimpleVariable :: Assertion
testSimpleVariable = do
  let var = Var (Var "x" 0)
  result <- case compileTerm defaultConfig var of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be x" "x" result

testVariableNamePreserved :: Assertion
testVariableNamePreserved = do
  let var = Var (Var "myVar" 42)  -- ID should be ignored in output
  result <- case compileTerm defaultConfig var of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be myVar" "myVar" result

-- ============================================================================
-- UNIT TESTS: CONSTANTS
-- ============================================================================

testIntConstant :: Assertion
testIntConstant = do
  let term = Const (IntLit 123)
  result <- case compileTerm defaultConfig term of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be 123" "123" result

testBoolConstant :: Assertion
testBoolConstant = do
  let term = Const (BoolLit True)
  result <- case compileTerm defaultConfig term of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be True" "True" result

testUnitConst :: Assertion
testUnitConst = do
  let term = Const UnitConst
  result <- case compileTerm defaultConfig term of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be ()" "()" result

-- ============================================================================
-- UNIT TESTS: LAMBDA
-- ============================================================================

testSimpleLambda :: Assertion
testSimpleLambda = do
  let lambda = Lam (Binder (Var "x" 0) Nothing) (Var (Var "x" 0))
  result <- case compileTerm defaultConfig lambda of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain backslash" ("\\" `T.isInfixOf` result)
  assertBool "Should contain x" ("x" `T.isInfixOf` result)

testLambdaBackslash :: Assertion
testLambdaBackslash = do
  let lambda = Lam (Binder (Var "f" 0) Nothing) (Var (Var "f" 0))
  result <- case compileTerm defaultConfig lambda of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should start with lambda" ("(\\" `T.isInfixOf` result)

testNestedLambda :: Assertion
testNestedLambda = do
  let inner = Lam (Binder (Var "y" 0) Nothing) (Var (Var "y" 0))
  let outer = Lam (Binder (Var "x" 0) Nothing) inner
  result <- case compileTerm defaultConfig outer of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain x" ("x" `T.isInfixOf` result)
  assertBool "Should contain y" ("y" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: APPLICATION
-- ============================================================================

testSimpleApplication :: Assertion
testSimpleApplication = do
  let f = Var (Var "f" 0)
  let x = Var (Var "x" 0)
  let app = App f x
  result <- case compileTerm defaultConfig app of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain f" ("f" `T.isInfixOf` result)
  assertBool "Should contain x" ("x" `T.isInfixOf` result)

testMultipleApplication :: Assertion
testMultipleApplication = do
  let f = Var (Var "f" 0)
  let x = Var (Var "x" 0)
  let y = Var (Var "y" 0)
  let app1 = App f x
  let app2 = App app1 y
  result <- case compileTerm defaultConfig app2 of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  -- Result should contain all three variables
  let hasAll = "f" `T.isInfixOf` result &&
               "x" `T.isInfixOf` result &&
               "y" `T.isInfixOf` result
  assertBool "Should contain all variables" hasAll

-- ============================================================================
-- UNIT TESTS: CONSTRUCTORS
-- ============================================================================

testSimpleConstructor :: Assertion
testSimpleConstructor = do
  let ctor = Constr (QName [] "True") []
  result <- case compileTerm defaultConfig ctor of
    Right x -> return x
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be True" "True" result

testConstructorWithArgs :: Assertion
testConstructorWithArgs = do
  let x = Var (Var "x" 0)
  let y = Var (Var "y" 0)
  let pair = Constr (QName ["Data", "Tuple"] "Pair") [x, y]
  result <- case compileTerm defaultConfig pair of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Pair" ("Pair" `T.isInfixOf` result)
  assertBool "Should contain x" ("x" `T.isInfixOf` result)
  assertBool "Should contain y" ("y" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: CASE EXPRESSIONS
-- ============================================================================

testSimpleCase :: Assertion
testSimpleCase = do
  let scrutinee = Var (Var "x" 0)
  let trueClause = Clause (PatConstr (QName [] "True") []) (Const (IntLit 1))
  let falseClause = Clause (PatConstr (QName [] "False") []) (Const (IntLit 0))
  let caseExpr = Case scrutinee [trueClause, falseClause] Nothing
  result <- case compileTerm defaultConfig caseExpr of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain case" ("case" `T.isInfixOf` result)
  assertBool "Should contain scrutinee" ("x" `T.isInfixOf` result)

testCaseWithDefault :: Assertion
testCaseWithDefault = do
  let scrutinee = Var (Var "x" 0)
  let defaultBody = Const (IntLit 42)
  let caseExpr = Case scrutinee [] (Just defaultBody)
  result <- case compileTerm defaultConfig caseExpr of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should be valid case" (not (T.null result))

-- ============================================================================
-- UNIT TESTS: LET-BINDINGS
-- ============================================================================

testSimpleLet :: Assertion
testSimpleLet = do
  let bindValue = Const (IntLit 5)
  let bodyExpr = Var (Var "x" 0)
  let letExpr = Let (Binder (Var "x" 0) Nothing) bindValue bodyExpr
  result <- case compileTerm defaultConfig letExpr of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain let" ("let" `T.isInfixOf` result)
  assertBool "Should contain 5" ("5" `T.isInfixOf` result)

testLetWithVar :: Assertion
testLetWithVar = do
  let bindValue = Const (IntLit 10)
  let bodyExpr = Var (Var "x" 0)
  let letExpr = Let (Binder (Var "x" 0) Nothing) bindValue bodyExpr
  result <- case compileTerm defaultConfig letExpr of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain in" ("in" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: ANNOTATIONS
-- ============================================================================

testAnnotatedTerm :: Assertion
testAnnotatedTerm = do
  let term = Var (Var "x" 0)
  let ty = TyConst (QName [] "Int") []
  let annTerm = Ann term ty
  result <- case compileTerm defaultConfig annTerm of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain variable" ("x" `T.isInfixOf` result)

-- ============================================================================
-- UNIT TESTS: PATTERNS
-- ============================================================================

testPatternVar :: Assertion
testPatternVar = do
  let pat = PatVar (Var "x" 0)
  result <- case compilePattern pat of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be x" "x" result

testPatternConstructor :: Assertion
testPatternConstructor = do
  let pat = PatConstr (QName [] "Cons")
              [ PatVar (Var "x" 0)
              , PatVar (Var "xs" 0)
              ]
  result <- case compilePattern pat of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertBool "Should contain Cons" ("Cons" `T.isInfixOf` result)
  assertBool "Should contain x" ("x" `T.isInfixOf` result)

testPatternWildcard :: Assertion
testPatternWildcard = do
  let pat = PatWildcard
  result <- case compilePattern pat of
    Right r -> return r
    Left err -> assertFailure $ "Unexpected error: " ++ err
  assertEqual "Should be wildcard" "_" result

-- ============================================================================
-- PROPERTY TESTS
-- ============================================================================

testDeterminism :: Assertion
testDeterminism = do
  let term = App (Var (Var "f" 0)) (Const (IntLit 42))
  let result1 = compileTerm defaultConfig term
  let result2 = compileTerm defaultConfig term
  assertEqual "Compilation should be deterministic" result1 result2
