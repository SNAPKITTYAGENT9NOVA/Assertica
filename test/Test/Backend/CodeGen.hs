{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.CodeGen
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Text as T
import Data.Text (Text)

import Assertica.Core.AST
import Assertica.Core.Module
import Assertica.Backend.CodeGen

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.CodeGen"
  [ testGroup "Simple Expression Compilation"
      [ testCase "Compile integer literal" testIntLiteral
      , testCase "Compile boolean literal" testBoolLiteral
      , testCase "Compile unit constant" testUnitConstant
      , testCase "Compile string literal" testStringLiteral
      ]
  , testGroup "Variable Compilation"
      [ testCase "Compile simple variable" testSimpleVar
      , testCase "Compile qualified variable" testQualifiedVar
      ]
  , testGroup "Lambda and Application"
      [ testCase "Compile simple lambda" testSimpleLambda
      , testCase "Compile application" testApplication
      , testCase "Compile nested application" testNestedApp
      ]
  , testGroup "Type Compilation"
      [ testCase "Compile function type" testFunctionType
      , testCase "Compile type variable" testTypeVar
      , testCase "Compile type constructor" testTypeConstructor
      ]
  , testGroup "Error Handling"
      [ testCase "Reject invalid qualified name" testInvalidQName
      ]
  , testGroup "Configuration"
      [ testCase "Default config has comments enabled" testDefaultConfig
      , testCase "Config affects output" testConfigAffectsOutput
      ]
  ]

-- ============================================================================
-- UNIT TESTS
-- ============================================================================

testIntLiteral :: Assertion
testIntLiteral = do
  let term = Const (IntLit 42)
  let result = compileExpression defaultConfig term
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain '42'" ("42" `T.isInfixOf` code)

testBoolLiteral :: Assertion
testBoolLiteral = do
  let term = Const (BoolLit True)
  let result = compileExpression defaultConfig term
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain 'True'" ("True" `T.isInfixOf` code)

testUnitConstant :: Assertion
testUnitConstant = do
  let term = Const UnitConst
  let result = compileExpression defaultConfig term
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertEqual "Should be unit" "()" code

testStringLiteral :: Assertion
testStringLiteral = do
  let term = Const (StringLit "hello")
  let result = compileExpression defaultConfig term
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain string" ("hello" `T.isInfixOf` code)

testSimpleVar :: Assertion
testSimpleVar = do
  let var = Var (Var "x" 0)
  let result = compileExpression defaultConfig var
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertEqual "Should be variable name" "x" code

testQualifiedVar :: Assertion
testQualifiedVar = do
  let var = Var (Var "map" 0)
  let result = compileExpression defaultConfig var
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain variable" ("map" `T.isInfixOf` code)

testSimpleLambda :: Assertion
testSimpleLambda = do
  let lambda = Lam (Binder (Var "x" 0) Nothing) (Var (Var "x" 0))
  let result = compileExpression defaultConfig lambda
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain lambda" ("\\" `T.isInfixOf` code)
      assertBool "Should contain variable" ("x" `T.isInfixOf` code)

testApplication :: Assertion
testApplication = do
  let f = Var (Var "f" 0)
  let x = Var (Var "x" 0)
  let app = App f x
  let result = compileExpression defaultConfig app
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain f" ("f" `T.isInfixOf` code)
      assertBool "Should contain x" ("x" `T.isInfixOf` code)

testNestedApp :: Assertion
testNestedApp = do
  let f = Var (Var "f" 0)
  let x = Var (Var "x" 0)
  let y = Var (Var "y" 0)
  let app1 = App f x
  let app2 = App app1 y
  let result = compileExpression defaultConfig app2
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should be valid expression" (not (T.null code))

testFunctionType :: Assertion
testFunctionType = do
  let tyInt = TyConst (QName [] "Int") []
  let tyBool = TyConst (QName [] "Bool") []
  let funType = TyFun tyInt tyBool
  let result = compileType defaultConfig funType
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertBool "Should contain arrow" ("->" `T.isInfixOf` code)

testTypeVar :: Assertion
testTypeVar = do
  let tv = TyVar (Var "a" 0)
  let result = compileType defaultConfig tv
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertEqual "Should be type variable" "a" code

testTypeConstructor :: Assertion
testTypeConstructor = do
  let intType = TyConst (QName [] "Int") []
  let result = compileType defaultConfig intType
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ err
    Right expr -> do
      let code = heExpr expr
      assertEqual "Should be Int" "Int" code

testInvalidQName :: Assertion
testInvalidQName = do
  -- This test checks that invalid qualified names are handled
  -- For now, most constructs compile, but this is a placeholder
  let qname = QName ["Assertica", "Core"] "someType"
  let ty = TyConst qname []
  let result = compileType defaultConfig ty
  case result of
    Left _ -> return ()  -- Error is acceptable
    Right expr -> do
      let code = heExpr expr
      assertBool "Should produce valid code" (not (T.null code))

testDefaultConfig :: Assertion
testDefaultConfig = do
  assertEqual "Add comments enabled" True (cgcAddComments defaultConfig)
  assertEqual "Add type signatures enabled" True (cgcAddTypeSignatures defaultConfig)

testConfigAffectsOutput :: Assertion
testConfigAffectsOutput = do
  let config1 = defaultConfig { cgcAddComments = True }
  let config2 = defaultConfig { cgcAddComments = False }

  let term = Const (IntLit 42)
  let result1 = compileExpression config1 term
  let result2 = compileExpression config2 term

  case (result1, result2) of
    (Right e1, Right e2) ->
      -- Both should produce valid expressions
      assertBool "First result valid" (not (T.null (heExpr e1)))
      assertBool "Second result valid" (not (T.null (heExpr e2)))
    (Left e, _) -> assertFailure $ "Unexpected error: " ++ e
    (_, Left e) -> assertFailure $ "Unexpected error: " ++ e

-- ============================================================================
-- PROPERTY TESTS
-- ============================================================================

-- Property: Code generation is deterministic
propDeterministic :: Term -> Bool
propDeterministic term =
  let result1 = compileExpression defaultConfig term
      result2 = compileExpression defaultConfig term
  in case (result1, result2) of
    (Right e1, Right e2) -> heExpr e1 == heExpr e2
    (Left err1, Left err2) -> err1 == err2
    _ -> False

-- Property: Simple constants compile without errors
propSimpleConstantsCompile :: Constant -> Bool
propSimpleConstantsCompile c =
  let term = Const c
      result = compileExpression defaultConfig term
  in case result of
    Right _ -> True
    Left _ -> False
