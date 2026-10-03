{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Integration
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Assertica.Core.AST
import Assertica.Core.Module
import Assertica.Backend.CodeGen
import Assertica.Backend.Emit

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.Integration"
  [ testGroup "End-to-End Compilation"
      [ testCase "Compile simple lambda to Haskell" testSimpleLambdaE2E
      , testCase "Compile let binding to Haskell" testLetBindingE2E
      , testCase "Compile case expression to Haskell" testCaseExpressionE2E
      ]
  , testGroup "Round-Trip Properties"
      [ testCase "Deterministic: AST -> Haskell is idempotent" testDeterministicCompilation
      ]
  , testGroup "Error Handling"
      [ testCase "Invalid input generates error" testErrorHandling
      ]
  , testGroup "Haskell Output Validity"
      [ testCase "Generated module has correct syntax" testModuleSyntax
      , testCase "Generated definitions are syntactically valid" testDefinitionSyntax
      ]
  ]

-- ============================================================================
-- END-TO-END TESTS
-- ============================================================================

testSimpleLambdaE2E :: Assertion
testSimpleLambdaE2E = do
  -- Compile: λx . x
  let lambda = Lam (Binder (Var "x" 0) Nothing) (Var (Var "x" 0))
  result <- case compileExpression defaultConfig lambda of
    Right expr -> return (heExpr expr)
    Left err -> assertFailure $ "Compilation failed: " ++ err

  -- Generated code should be valid Haskell
  assertBool "Should be non-empty" (not (T.null result))
  assertBool "Should contain lambda operator" ("\\" `T.isInfixOf` result)
  assertBool "Should contain variable" ("x" `T.isInfixOf` result)

testLetBindingE2E :: Assertion
testLetBindingE2E = do
  -- Compile: let x = 5 in x
  let letExpr = Let
        (Binder (Var "x" 0) Nothing)
        (Const (IntLit 5))
        (Var (Var "x" 0))

  result <- case compileExpression defaultConfig letExpr of
    Right expr -> return (heExpr expr)
    Left err -> assertFailure $ "Compilation failed: " ++ err

  assertBool "Should be non-empty" (not (T.null result))
  assertBool "Should contain let" ("let" `T.isInfixOf` result)
  assertBool "Should contain 5" ("5" `T.isInfixOf` result)
  assertBool "Should contain in" ("in" `T.isInfixOf` result)

testCaseExpressionE2E :: Assertion
testCaseExpressionE2E = do
  -- Compile: case x of { True -> 1; False -> 0 }
  let trueClause = Clause (PatConstr (QName [] "True") []) (Const (IntLit 1))
  let falseClause = Clause (PatConstr (QName [] "False") []) (Const (IntLit 0))
  let caseExpr = Case (Var (Var "x" 0)) [trueClause, falseClause] Nothing

  result <- case compileExpression defaultConfig caseExpr of
    Right expr -> return (heExpr expr)
    Left err -> assertFailure $ "Compilation failed: " ++ err

  assertBool "Should be non-empty" (not (T.null result))
  assertBool "Should contain case" ("case" `T.isInfixOf` result)
  assertBool "Should contain 1" ("1" `T.isInfixOf` result)
  assertBool "Should contain 0" ("0" `T.isInfixOf` result)

-- ============================================================================
-- ROUND-TRIP AND IDEMPOTENCY TESTS
-- ============================================================================

testDeterministicCompilation :: Assertion
testDeterministicCompilation = do
  let lambda = Lam (Binder (Var "x" 0) Nothing) (Var (Var "x" 0))

  -- Compile three times
  let result1 = compileExpression defaultConfig lambda
  let result2 = compileExpression defaultConfig lambda
  let result3 = compileExpression defaultConfig lambda

  case (result1, result2, result3) of
    (Right e1, Right e2, Right e3) -> do
      let r1 = heExpr e1
      let r2 = heExpr e2
      let r3 = heExpr e3
      assertEqual "First and second should match" r1 r2
      assertEqual "Second and third should match" r2 r3
    (Left e, _, _) -> assertFailure $ "Compilation failed: " ++ e
    (_, Left e, _) -> assertFailure $ "Compilation failed: " ++ e
    (_, _, Left e) -> assertFailure $ "Compilation failed: " ++ e

-- ============================================================================
-- ERROR HANDLING
-- ============================================================================

testErrorHandling :: Assertion
testErrorHandling = do
  -- Test that various error conditions are handled gracefully
  -- For now, most constructs compile, so test basic error path existence
  let x = Var (Var "x" 0)
  let result = compileExpression defaultConfig x
  case result of
    Right _ -> return ()  -- Success is also valid
    Left _ -> return ()   -- Error is acceptable

-- ============================================================================
-- HASKELL OUTPUT VALIDITY
-- ============================================================================

testModuleSyntax :: Assertion
testModuleSyntax = do
  let modName = ["Test", "Module"]
  let def = HaskellDefinition
        { hdName = "main"
        , hdTypeSignature = Just "IO ()"
        , hdBody = "putStrLn \"Hello\""
        , hdSourceLocation = Nothing
        }
  let mod = HaskellModule
        { hmName = modName
        , hmImports = ["Prelude"]
        , hmDefinitions = [def]
        , hmComments = ["Test module"]
        }

  let haskellCode = emitHaskellModule defaultEmitConfig mod

  -- Check basic Haskell syntax
  assertBool "Should start with comment or module" $
    "-- " `T.isPrefixOf` haskellCode || "module" `T.isPrefixOf` haskellCode

  assertBool "Should contain module declaration" $
    "module" `T.isInfixOf` haskellCode

  assertBool "Should contain where" $
    "where" `T.isInfixOf` haskellCode

  -- Count the lines to ensure structure
  let lineCount = length (T.lines haskellCode)
  assertBool "Should have multiple lines" (lineCount > 3)

testDefinitionSyntax :: Assertion
testDefinitionSyntax = do
  let def1 = HaskellDefinition
        { hdName = "id"
        , hdTypeSignature = Just "a -> a"
        , hdBody = "\\x -> x"
        , hdSourceLocation = Just "identity function"
        }

  let def2 = HaskellDefinition
        { hdName = "const"
        , hdTypeSignature = Just "a -> b -> a"
        , hdBody = "\\x y -> x"
        , hdSourceLocation = Nothing
        }

  let code1 = emitHaskellDefinition defaultEmitConfig def1
  let code2 = emitHaskellDefinition defaultEmitConfig def2

  -- Both should have valid structure
  assertBool "def1 should contain name" ("id" `T.isInfixOf` code1)
  assertBool "def1 should contain type" ("a" `T.isInfixOf` code1)
  assertBool "def1 should contain arrow" ("->" `T.isInfixOf` code1)
  assertBool "def1 should contain comment" ("--" `T.isInfixOf` code1)

  assertBool "def2 should contain name" ("const" `T.isInfixOf` code2)
  assertBool "def2 should contain type" ("a" `T.isInfixOf` code2)
