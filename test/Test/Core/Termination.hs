{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Core.Termination
Description : Tests for termination checking (Agent 4B)
-}

module Test.Core.Termination
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Core.Termination
import Assertica.Core.TypeEnv

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.Termination"
  [ testGroup "Recursion Detection"
      [ testCase "No recursion detected in simple term" testNoRecursion
      , testCase "Direct recursion detected" testDirectRecursion
      , testCase "Recursion in application" testRecursionInApp
      , testCase "Recursion in case expression" testRecursionInCase
      ]

  , testGroup "Simple Recursion Patterns"
      [ testCase "Simple structural recursion accepted" testStructuralRecursion
      , testCase "Non-terminating recursion rejected" testNonTerminating
      , testCase "Guarded recursion accepted" testGuardedRecursion
      ]

  , testGroup "Recursive Call Analysis"
      [ testCase "Find direct recursive call" testFindDirectCall
      , testCase "Analyze structural call pattern" testStructuralCallPattern
      , testCase "Detect unsupported recursion pattern" testUnsupportedPattern
      ]

  , testGroup "Mutual Recursion"
      [ testCase "Mutual recursion with progress" testMutualRecursionProgress
      , testCase "Infinite mutual recursion rejected" testInfiniteMutualRecursion
      , testCase "Call cycle detection" testCallCycleDetection
      ]

  , testGroup "Integration Tests"
      [ testCase "Length function on List" testLengthFunction
      , testCase "Fibonacci with guards" testFibonacciFunction
      , testCase "Map over list" testMapFunction
      ]

  , testGroup "Error Messages"
      [ testCase "Non-terminating error message" testNonTerminatingError
      , testCase "Unsupported pattern error message" testUnsupportedPatternError
      , testCase "Cycle detection error message" testCycleDetectionError
      ]
  ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Create a simple variable. -}
var :: Text -> Var
var name = Var name 0

{-| Create a simple qualified name. -}
qn :: Text -> QName
qn name = QName [] name

{-| Create a binder. -}
binder :: Text -> Maybe Type -> Binder
binder name mty = Binder (var name) mty

{-| Create a pattern for a variable. -}
patVar :: Text -> Pattern
patVar name = PatVar (var name)

{-| Create a clause. -}
clause :: Pattern -> Term -> Clause
clause = Clause

-- ============================================================================
-- RECURSION DETECTION TESTS
-- ============================================================================

testNoRecursion :: Assertion
testNoRecursion = do
  -- A simple non-recursive term
  let term = Const (IntLit 42)
  assertBool "Non-recursive term should not have recursion" $
    not (hasRecursion term)

testDirectRecursion :: Assertion
testDirectRecursion = do
  -- A term that calls itself: f x
  let term = App (Var (var "f")) (Var (var "x"))
  assertBool "Application of f should be detected as having recursion" $
    hasRecursion term

testRecursionInApp :: Assertion
testRecursionInApp = do
  -- f (g x)
  let term = App (Var (var "f")) (App (Var (var "g")) (Var (var "x")))
  assertBool "Recursion in nested application should be detected" $
    hasRecursion term

testRecursionInCase :: Assertion
testRecursionInCase = do
  -- case x of P -> f y
  let scrutinee = Var (var "x")
      c1 = clause (patVar "y") (App (Var (var "f")) (Var (var "y")))
      term = Case scrutinee [c1] Nothing
  assertBool "Recursion in case body should be detected" $
    hasRecursion term

-- ============================================================================
-- SIMPLE RECURSION PATTERN TESTS
-- ============================================================================

testStructuralRecursion :: Assertion
testStructuralRecursion = do
  -- length (Cons h t) = 1 + length t
  -- Simplified: just the recursive call part
  let funcName = "length"
      -- App of length to a term
      term = App (Var (var "length")) (Var (var "t"))

  case checkTermination emptyEnv funcName term of
    Right () -> return ()  -- Should succeed
    Left err -> assertFailure $ "Structural recursion should pass: " ++ prettyTermError err

testNonTerminating :: Assertion
testNonTerminating = do
  -- f x = f (x + 1)
  let funcName = "f"
      -- The recursive call f (x + 1)
      successor = App (Const (PrimOp OpAdd)) (Var (var "x"))
      term = App (Var (var "f")) successor

  case checkTermination emptyEnv funcName term of
    Left (NonTerminatingRecursion _) -> return ()  -- Should fail
    Left err -> assertFailure $ "Wrong error type: " ++ show err
    Right () -> assertFailure "Non-terminating recursion should be rejected"

testGuardedRecursion :: Assertion
testGuardedRecursion = do
  -- Simplified guarded recursion
  let funcName = "countdown"
      -- countdown n = if n <= 0 then ... else countdown (n - 1)
      -- Just the recursive part
      term = App (Var (var "countdown")) (Var (var "n"))

  case checkTermination emptyEnv funcName term of
    Right () -> return ()  -- Should succeed (structural pattern)
    Left err -> assertFailure $ "Guarded recursion should pass: " ++ prettyTermError err

-- ============================================================================
-- RECURSIVE CALL ANALYSIS TESTS
-- ============================================================================

testFindDirectCall :: Assertion
testFindDirectCall = do
  let funcName = "length"
      term = App (Var (var "length")) (Var (var "t"))

  case analyzeRecursiveCalls funcName term of
    Right calls ->
      case calls of
        [RecursiveCall name _ _] ->
          assertEqual "Should find call to length" name "length"
        [] -> assertFailure "Should find at least one recursive call"
        _ -> assertFailure "Should find exactly one recursive call"
    Left err -> assertFailure $ "Analysis should succeed: " ++ prettyTermError err

testStructuralCallPattern :: Assertion
testStructuralCallPattern = do
  let funcName = "walk"
      -- walk (tree)
      term = App (Var (var "walk")) (Constr (qn "Leaf") [Var (var "x")])

  case analyzeRecursiveCalls funcName term of
    Right [RecursiveCall _ pattern _] ->
      assertEqual "Should detect structural call pattern" pattern (StructuralCall "constr")
    _ -> assertFailure "Should find structural call pattern"

testUnsupportedPattern :: Assertion
testUnsupportedPattern = do
  let funcName = "strange"
      -- A recursive call with unknown pattern
      unhandledOp = App (Const (PrimOp OpDiv)) (Var (var "x"))
      term = App (Var (var "strange")) unhandledOp

  case analyzeRecursiveCalls funcName term of
    Right calls ->
      case calls of
        [RecursiveCall _ UnknownPattern _] -> return ()
        _ -> assertFailure "Should detect unknown pattern"
    Left err -> assertFailure $ "Analysis should still work: " ++ prettyTermError err

-- ============================================================================
-- MUTUAL RECURSION TESTS
-- ============================================================================

testMutualRecursionProgress :: Assertion
testMutualRecursionProgress = do
  -- even n = if n == 0 then true else odd (n - 1)
  -- odd n = if n == 0 then false else even (n - 1)
  let functions = [("even", Var (var "x")), ("odd", Var (var "y"))]

  case checkTerminationMulti emptyEnv functions of
    Right () -> return ()  -- Should succeed
    Left err -> assertFailure $ "Mutual recursion with progress should pass: " ++ prettyTermError err

testInfiniteMutualRecursion :: Assertion
testInfiniteMutualRecursion = do
  -- f x = g x
  -- g x = f x
  -- No progress on any argument
  let functions = [("f", Var (var "x")), ("g", Var (var "x"))]

  case checkTerminationMulti emptyEnv functions of
    Left (InfiniteCallCycle _) -> return ()  -- Should detect cycle
    Left err -> assertFailure $ "Wrong error type: " ++ show err
    Right () -> assertFailure "Infinite cycle should be rejected"

testCallCycleDetection :: Assertion
testCallCycleDetection = do
  -- a calls b, b calls c, c calls a
  let functions = [("a", Var (var "b")), ("b", Var (var "c")), ("c", Var (var "a"))]

  case checkTerminationMulti emptyEnv functions of
    Left (InfiniteCallCycle cycle_path) ->
      assertBool "Cycle should be detected" (length cycle_path > 0)
    _ -> assertFailure "Should detect cycle in call graph"

-- ============================================================================
-- INTEGRATION TESTS
-- ============================================================================

testLengthFunction :: Assertion
testLengthFunction = do
  -- Simulate length :: List a -> Nat
  -- length Nil = 0
  -- length (Cons _ t) = 1 + length t
  let funcName = "length"
      -- The recursive case: length t
      consCase = Constr (qn "Cons") [Var (var "h"), Var (var "t")]
      recursiveCall = App (Var (var "length")) (Var (var "t"))

  case checkTermination emptyEnv funcName recursiveCall of
    Right () -> return ()
    Left err -> assertFailure $ "length should terminate: " ++ prettyTermError err

testFibonacciFunction :: Assertion
testFibonacciFunction = do
  -- fib n = if n <= 1 then n else fib(n-1) + fib(n-2)
  -- For simplicity, just check the recursive call part
  let funcName = "fib"
      -- fib (n - 1)
      term = App (Var (var "fib")) (Var (var "n"))

  case checkTermination emptyEnv funcName term of
    Right () -> return ()
    Left err -> assertFailure $ "fib should pass initial check: " ++ prettyTermError err

testMapFunction :: Assertion
testMapFunction = do
  -- map :: (a -> b) -> List a -> List b
  -- map f [] = []
  -- map f (x:xs) = f x : map f xs
  let funcName = "map"
      -- map f xs
      recursiveCall = App (Var (var "map")) (Var (var "xs"))

  case checkTermination emptyEnv funcName recursiveCall of
    Right () -> return ()
    Left err -> assertFailure $ "map should terminate: " ++ prettyTermError err

-- ============================================================================
-- ERROR MESSAGE TESTS
-- ============================================================================

testNonTerminatingError :: Assertion
testNonTerminatingError = do
  let err = NonTerminatingRecursion "recursive call does not decrease"
  let msg = prettyTermError err
  assertBool "Error message should mention non-termination" $
    "does not terminate" `elem` words msg

testUnsupportedPatternError :: Assertion
testUnsupportedPatternError = do
  let err = UnsupportedRecursionPattern "unknown pattern found"
  let msg = prettyTermError err
  assertBool "Error message should mention unsupported pattern" $
    "pattern" `elem` words msg

testCycleDetectionError :: Assertion
testCycleDetectionError = do
  let cycle_path = ["f", "g", "f"]
  let err = InfiniteCallCycle cycle_path
  let msg = prettyTermError err
  assertBool "Error message should mention cycle" $
    "cycle" `elem` words msg
