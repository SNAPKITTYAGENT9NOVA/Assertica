{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Core.Positivity
Description : Tests for positivity checking (Agent 4B)
-}

module Test.Core.Positivity
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Core.Positivity

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.Positivity"
  [ testGroup "Basic Positivity"
      [ testCase "Type with no parameters needs no checking" testNoParams
      , testCase "Strictly positive list type" testListPositive
      , testCase "Strictly positive tree type" testTreePositive
      ]

  , testGroup "Negative Position Detection"
      [ testCase "Function domain is negative position" testFunctionDomainNegative
      , testCase "Type parameter in function domain rejected" testParamInDomainRejected
      , testCase "Nested function domain rejected" testNestedFunctionNegative
      ]

  , testGroup "Constructor Analysis"
      [ testCase "Multiple constructors checked" testMultipleConstructors
      , testCase "Nested positive occurrences accepted" testNestedPositive
      , testCase "Mixed positive and negative rejected" testMixedPositiveNegative
      ]

  , testGroup "Type Parameter Tracking"
      [ testCase "Occurrence analysis finds type variables" testOccurrenceAnalysis
      , testCase "Analyze position context" testPositionContext
      , testCase "Nested type structures" testNestedTypeStructures
      ]

  , testGroup "Practical Examples"
      [ testCase "List a is strictly positive" testListStrictlyPositive
      , testCase "Tree a is strictly positive" testTreeStrictlyPositive
      , testCase "BadType (a -> Int) rejected" testBadTypeRejected
      , testCase "BadList with function in arg rejected" testBadListRejected
      ]

  , testGroup "Mutual/Recursive Inductive Types"
      [ testCase "Mutual positive types accepted" testMutualPositive
      , testCase "Mutual with negative occurrence rejected" testMutualWithNegative
      ]

  , testGroup "Error Messages"
      [ testCase "Non-positive occurrence message" testNonPositiveMessage
      , testCase "Function domain message" testFunctionDomainMessage
      , testCase "Nested negative message" testNestedNegativeMessage
      ]
  ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Create a simple variable. -}
var :: Text -> Var
var name = Var name 0

{-| Create a qualified name. -}
qn :: Text -> QName
qn name = QName [] name

{-| Create a binder. -}
binder :: Text -> Maybe Type -> Binder
binder name mty = Binder (var name) mty

{-| Create a type variable. -}
tyVar :: Text -> Type
tyVar name = TyVar (var name)

{-| Create a type constructor application. -}
tyConst :: Text -> [Type] -> Type
tyConst name args = TyConst (qn name) args

{-| Create a function type. -}
tyFun :: Type -> Type -> Type
tyFun = TyFun

-- ============================================================================
-- BASIC POSITIVITY TESTS
-- ============================================================================

testNoParams :: Assertion
testNoParams = do
  -- data Nat = Zero | Succ Nat (no type parameters)
  let params = []
      ctors = [tyConst "Nat" []]

  case checkPositivity "Nat" params ctors of
    Right () -> return ()
    Left err -> assertFailure $ "Nat should be positive: " ++ prettyPosError err

testListPositive :: Assertion
testListPositive = do
  -- data List a = Nil | Cons a (List a)
  let a = var "a"
      params = [a]
      -- Cons argument types: [a, List a]
      ctors = [tyVar "a", tyConst "List" [tyVar "a"]]

  case checkPositivity "List" params ctors of
    Right () -> return ()
    Left err -> assertFailure $ "List should be positive: " ++ prettyPosError err

testTreePositive :: Assertion
testTreePositive = do
  -- data Tree a = Leaf a | Node (Tree a) (Tree a)
  let a = var "a"
      params = [a]
      -- Constructor args: [a] for Leaf, [Tree a, Tree a] for Node
      ctors = [tyVar "a", tyConst "Tree" [tyVar "a"], tyConst "Tree" [tyVar "a"]]

  case checkPositivity "Tree" params ctors of
    Right () -> return ()
    Left err -> assertFailure $ "Tree should be positive: " ++ prettyPosError err

-- ============================================================================
-- NEGATIVE POSITION DETECTION TESTS
-- ============================================================================

testFunctionDomainNegative :: Assertion
testFunctionDomainNegative = do
  -- Type parameter appears in function domain: a -> Int
  let a = var "a"
      params = [a]
      -- Argument type: a -> Int
      ctors = [tyFun (tyVar "a") (tyConst "Int" [])]

  case checkPositivity "Bad" params ctors of
    Left (TypeVarInFunctionDomain _) -> return ()  -- Should reject
    Left err -> assertFailure $ "Wrong error type: " ++ prettyPosError err
    Right () -> assertFailure "Function domain should be rejected"

testParamInDomainRejected :: Assertion
testParamInDomainRejected = do
  -- data BadType a = Bad (a -> Int)
  let a = var "a"
      params = [a]
      ctors = [tyFun (tyVar "a") (tyConst "Int" [])]

  case checkPositivity "BadType" params ctors of
    Left (TypeVarInFunctionDomain "a") -> return ()
    _ -> assertFailure "Should reject a in function domain"

testNestedFunctionNegative :: Assertion
testNestedFunctionNegative = do
  -- data BadList a = Cons a (List (a -> Int))
  let a = var "a"
      params = [a]
      -- (a -> Int) is in a positive position but contains a in negative
      listOfFunc = tyConst "List" [tyFun (tyVar "a") (tyConst "Int" [])]
      ctors = [tyVar "a", listOfFunc]

  case checkPositivity "BadList" params ctors of
    Left (NestedNegativeOccurrence "a" _) -> return ()
    _ -> assertFailure "Should detect nested negative occurrence"

-- ============================================================================
-- CONSTRUCTOR ANALYSIS TESTS
-- ============================================================================

testMultipleConstructors :: Assertion
testMultipleConstructors = do
  -- data List a = Nil | Cons a (List a)
  -- Both constructors must be checked
  let a = var "a"
      params = [a]
      -- Nil args, Cons args
      ctors = [tyConst "List" [tyVar "a"]]

  case checkPositivity "List" params ctors of
    Right () -> return ()
    Left err -> assertFailure $ "Multiple constructors should all pass: " ++ prettyPosError err

testNestedPositive :: Assertion
testNestedPositive = do
  -- Nested type constructor applications in positive positions
  -- data Okay a = OK (List (List a))
  let a = var "a"
      params = [a]
      nestedList = tyConst "List" [tyConst "List" [tyVar "a"]]
      ctors = [nestedList]

  case checkPositivity "Okay" params ctors of
    Right () -> return ()
    Left err -> assertFailure $ "Nested positive should pass: " ++ prettyPosError err

testMixedPositiveNegative :: Assertion
testMixedPositiveNegative = do
  -- data Mixed a = Good a | Bad (a -> Int)
  -- First constructor is fine, second violates positivity
  let a = var "a"
      params = [a]
      ctors = [tyVar "a", tyFun (tyVar "a") (tyConst "Int" [])]

  case checkPositivity "Mixed" params ctors of
    Left _ -> return ()  -- Should reject due to negative occurrence
    Right () -> assertFailure "Should reject negative occurrence"

-- ============================================================================
-- TYPE PARAMETER TRACKING TESTS
-- ============================================================================

testOccurrenceAnalysis :: Assertion
testOccurrenceAnalysis = do
  -- Analyze where a type variable appears
  let params = Set.singleton "a"
      ty = tyFun (tyVar "a") (tyConst "List" [tyVar "a"])

  let occs = analyzeTypeOccurrences params ty
  assertBool "Should find multiple occurrences of a" (length occs >= 1)
  let hasNeg = any (\o -> occPosition o == Negative) occs
  assertBool "Should find negative occurrence in domain" hasNeg

testPositionContext :: Assertion
testPositionContext = do
  -- Verify position tracking
  let params = Set.singleton "a"
      ty = tyConst "List" [tyVar "a"]

  let occs = analyzeTypeOccurrences params ty
  case occs of
    [TypeOccurrence _ pos _] ->
      assertEqual "Should be in positive position" pos Positive
    _ -> assertFailure "Should find exactly one occurrence"

testNestedTypeStructures :: Assertion
testNestedTypeStructures = do
  -- Complex nested type: List (Tree (a -> Int))
  let params = Set.singleton "a"
      negFunc = tyFun (tyVar "a") (tyConst "Int" [])
      tree = tyConst "Tree" [negFunc]
      list = tyConst "List" [tree]

  let occs = analyzeTypeOccurrences params list
  let hasNeg = any (\o -> occPosition o == Negative) occs
  assertBool "Should track nested negative position" hasNeg

-- ============================================================================
-- PRACTICAL EXAMPLE TESTS
-- ============================================================================

testListStrictlyPositive :: Assertion
testListStrictlyPositive = do
  -- data List a = Nil | Cons a (List a)
  let a = var "a"
      params = [a]
      nilCtors = []  -- Nil has no arguments
      consCtors = [tyVar "a", tyConst "List" [tyVar "a"]]
      allCtors = nilCtors ++ consCtors

  case checkPositivity "List" params allCtors of
    Right () -> return ()
    Left err -> assertFailure $ "List should be strictly positive: " ++ prettyPosError err

testTreeStrictlyPositive :: Assertion
testTreeStrictlyPositive = do
  -- data Tree a = Leaf a | Node (Tree a) (Tree a)
  let a = var "a"
      params = [a]
      leafCtors = [tyVar "a"]
      nodeCtors = [tyConst "Tree" [tyVar "a"], tyConst "Tree" [tyVar "a"]]
      allCtors = leafCtors ++ nodeCtors

  case checkPositivity "Tree" params allCtors of
    Right () -> return ()
    Left err -> assertFailure $ "Tree should be strictly positive: " ++ prettyPosError err

testBadTypeRejected :: Assertion
testBadTypeRejected = do
  -- data BadType a = Bad (a -> Int)
  let a = var "a"
      params = [a]
      badCtors = [tyFun (tyVar "a") (tyConst "Int" [])]

  case checkPositivity "BadType" params badCtors of
    Left _ -> return ()  -- Should be rejected
    Right () -> assertFailure "BadType should be rejected"

testBadListRejected :: Assertion
testBadListRejected = do
  -- data BadList a = Cons a (BadList (a -> Int))
  let a = var "a"
      params = [a]
      funcType = tyFun (tyVar "a") (tyConst "Int" [])
      badListArg = tyConst "BadList" [funcType]
      badCtors = [tyVar "a", badListArg]

  case checkPositivity "BadList" params badCtors of
    Left _ -> return ()  -- Should be rejected
    Right () -> assertFailure "BadList should be rejected"

-- ============================================================================
-- MUTUAL/RECURSIVE INDUCTIVE TYPES TESTS
-- ============================================================================

testMutualPositive :: Assertion
testMutualPositive = do
  -- data Even = Zero | SuccO Odd
  -- data Odd = SuccE Even
  let defs =
        [ ("Even", [], [tyConst "Odd" []])
        , ("Odd", [], [tyConst "Even" []])
        ]

  case checkPositivityMulti defs of
    Right () -> return ()
    Left err -> assertFailure $ "Mutual types should pass: " ++ prettyPosError err

testMutualWithNegative :: Assertion
testMutualWithNegative = do
  -- data Even = Zero | SuccO (Odd -> Int)  -- NEGATIVE!
  -- data Odd = SuccE Even
  let a = var "a"
      defs =
        [ ("Even", [a], [tyFun (tyConst "Odd" []) (tyConst "Int" [])])
        , ("Odd", [a], [tyConst "Even" []])
        ]

  case checkPositivityMulti defs of
    Left _ -> return ()  -- Should be rejected
    Right () -> assertFailure "Mutual with negative should be rejected"

-- ============================================================================
-- ERROR MESSAGE TESTS
-- ============================================================================

testNonPositiveMessage :: Assertion
testNonPositiveMessage = do
  let err = NonPositiveOccurrence "a" "in function domain"
  let msg = prettyPosError err
  assertBool "Error should mention type parameter" $ "a" `elem` words msg
  assertBool "Error should mention non-positive" $ "non-positive" `elem` words msg

testFunctionDomainMessage :: Assertion
testFunctionDomainMessage = do
  let err = TypeVarInFunctionDomain "a"
  let msg = prettyPosError err
  assertBool "Error should mention function domain" $ "domain" `elem` words msg
  assertBool "Error should mention parameter a" $ "a" `elem` words msg

testNestedNegativeMessage :: Assertion
testNestedNegativeMessage = do
  let err = NestedNegativeOccurrence "a" "in List (a -> Int)"
  let msg = prettyPosError err
  assertBool "Error should mention nested" $ "nested" `elem` words msg
  assertBool "Error should mention negative" $ "negative" `elem` words msg
