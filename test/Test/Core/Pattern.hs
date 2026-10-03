{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Core.Pattern
Description : Unit tests for the pattern compiler (Agent 4A)
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Tests for:
- Pattern AST construction and validation
- Constructor database queries
- Exhaustiveness checking
- Pattern compilation to core Case expressions
- Type-aware pattern matching
- Error diagnostics
-}

module Test.Core.Pattern
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Core.Pattern

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Create a simple QName from module path and local name.
-}
qname :: [Text] -> Text -> QName
qname = QName

{-| Create a variable with given name and ID.
-}
var :: Text -> Int -> Var
var name id = Var name id

{-| Create a type constant (e.g., List, Bool, Int).
-}
typeCon :: [Text] -> Text -> Type
typeCon mod name = TyConst (qname mod name) []

{-| Create a type constant with arguments.
-}
typeConApp :: [Text] -> Text -> [Type] -> Type
typeConApp mod name args = TyConst (qname mod name) args

-- ============================================================================
-- UNIT TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.Pattern"
  [ testGroup "Constructor Database"
      [ testCase "Empty database" testEmptyDB
      , testCase "Add constructor" testAddConstructor
      , testCase "Query constructors" testQueryConstructors
      , testCase "Query unknown type" testQueryUnknownType
      ]

  , testGroup "Pattern Compilation"
      [ testCase "Compile variable pattern" testCompileVarPattern
      , testCase "Compile wildcard pattern" testCompileWildcardPattern
      , testCase "Compile constructor pattern" testCompileConstrPattern
      , testCase "Compile nested patterns" testCompileNestedPatterns
      , testCase "Extract pattern bindings" testExtractBindings
      ]

  , testGroup "Exhaustiveness Checking"
      [ testCase "Default clause is exhaustive" testDefaultExhaustive
      , testCase "Empty patterns with default" testEmptyPatternsWithDefault
      , testCase "Single wildcard is exhaustive" testWildcardExhaustive
      ]

  , testGroup "First Milestone: List Pattern Matching"
      [ testCase "List type with Nil and Cons" testListTypeDB
      , testCase "Simple list patterns compile" testSimpleListPatterns
      , testCase "List patterns are exhaustive" testListPatternsExhaustive
      ]

  , testGroup "Pattern Type Inference"
      [ testCase "Infer variable pattern type" testInferVarType
      , testCase "Infer constructor pattern type" testInferConstrType
      ]

  , testGroup "Error Handling"
      [ testCase "Unknown constructor error" testUnknownConstructorError
      , testCase "Arity mismatch error" testArityMismatchError
      ]

  , testGroup "Pattern Utilities"
      [ testCase "Is wildcard pattern" testIsWildcardPattern
      , testCase "Extract constructors from pattern" testPatternConstructors
      , testCase "Check redundant patterns" testRedundantPatterns
      ]
  ]

-- ============================================================================
-- CONSTRUCTOR DATABASE TESTS
-- ============================================================================

testEmptyDB :: Assertion
testEmptyDB =
  emptyConstructorDB @?= Map.empty

testAddConstructor :: Assertion
testAddConstructor =
  let db = emptyConstructorDB
      listTy = qname [] "List"
      nilCtor = Constructor
        { ctorName = qname [] "Nil"
        , ctorArity = 0
        , ctorArgTypes = []
        , ctorReturnType = typeConApp [] "List" [TyVar (var "a" 0)]
        }
      db' = addConstructor db listTy nilCtor
  in Map.size db' @?= 1

testQueryConstructors :: Assertion
testQueryConstructors =
  let db = emptyConstructorDB
      listTy = qname [] "List"
      nilCtor = Constructor
        { ctorName = qname [] "Nil"
        , ctorArity = 0
        , ctorArgTypes = []
        , ctorReturnType = typeConApp [] "List" [TyVar (var "a" 0)]
        }
      db' = addConstructor db listTy nilCtor
  in case queryConstructors db' listTy of
       Right ctors -> length ctors @?= 1
       Left _ -> assertFailure "Query failed"

testQueryUnknownType :: Assertion
testQueryUnknownType =
  let db = emptyConstructorDB
      unknownTy = qname [] "Unknown"
  in case queryConstructors db unknownTy of
       Right _ -> assertFailure "Should fail for unknown type"
       Left (UnknownConstructor _) -> return ()
       Left err -> assertFailure $ "Wrong error: " ++ show err

-- ============================================================================
-- PATTERN COMPILATION TESTS
-- ============================================================================

testCompileVarPattern :: Assertion
testCompileVarPattern =
  let pat = PatVar (var "x" 0)
  in case compilePattern pat of
       Right cp -> do
         cpOriginal cp @?= pat
         Map.size (cpBindings cp) @?= 1
       Left err -> assertFailure $ "Compilation failed: " ++ prettyPatternError err

testCompileWildcardPattern :: Assertion
testCompileWildcardPattern =
  let pat = PatWildcard
  in case compilePattern pat of
       Right cp -> do
         cpOriginal cp @?= pat
         Map.size (cpBindings cp) @?= 0
       Left err -> assertFailure $ "Compilation failed: " ++ prettyPatternError err

testCompileConstrPattern :: Assertion
testCompileConstrPattern =
  let pat = PatConstr (qname [] "Cons") [PatVar (var "h" 0), PatVar (var "t" 1)]
  in case compilePattern pat of
       Right cp -> do
         cpOriginal cp @?= pat
         Map.size (cpBindings cp) @?= 2
       Left err -> assertFailure $ "Compilation failed: " ++ prettyPatternError err

testCompileNestedPatterns :: Assertion
testCompileNestedPatterns =
  let pat = PatConstr (qname [] "Cons")
              [PatVar (var "h" 0), PatConstr (qname [] "Cons") [PatVar (var "h2" 1), PatWildcard]]
  in case compilePattern pat of
       Right cp -> do
         cpOriginal cp @?= pat
         Map.size (cpBindings cp) @?= 2
       Left err -> assertFailure $ "Compilation failed: " ++ prettyPatternError err

testExtractBindings :: Assertion
testExtractBindings =
  let pat = PatConstr (qname [] "Cons") [PatVar (var "h" 0), PatVar (var "t" 1)]
      ty = typeConApp [] "List" [typeCon [] "Int"]
      bindings = extractPatternBindings pat ty
  in Map.size bindings @?= 2

-- ============================================================================
-- EXHAUSTIVENESS CHECKING TESTS
-- ============================================================================

testDefaultExhaustive :: Assertion
testDefaultExhaustive =
  let db = emptyConstructorDB
      ty = typeCon [] "Bool"
      patterns = [PatConstr (qname [] "True") []]
      defClause = Just PatWildcard
  in case checkExhaustive db ty patterns defClause of
       Right () -> return ()
       Left err -> assertFailure $ "Should be exhaustive: " ++ prettyExhaustiveError err

testEmptyPatternsWithDefault :: Assertion
testEmptyPatternsWithDefault =
  let db = emptyConstructorDB
      ty = typeCon [] "Any"
      patterns = []
      defClause = Just PatWildcard
  in case checkExhaustive db ty patterns defClause of
       Right () -> return ()
       Left err -> assertFailure $ "Should be exhaustive: " ++ prettyExhaustiveError err

testWildcardExhaustive :: Assertion
testWildcardExhaustive =
  let db = emptyConstructorDB
      ty = typeCon [] "Any"
      patterns = [PatWildcard]
      defClause = Nothing
  in case checkExhaustive db ty patterns defClause of
       Right () -> return ()
       Left err -> assertFailure $ "Should be exhaustive: " ++ prettyExhaustiveError err

-- ============================================================================
-- FIRST MILESTONE: LIST PATTERN MATCHING
-- ============================================================================

{-| Set up a constructor database with List type containing Nil and Cons.
-}
mkListDB :: ConstructorDB
mkListDB =
  let db = emptyConstructorDB
      listTy = qname [] "List"

      -- Nil : List a
      nilCtor = Constructor
        { ctorName = qname [] "Nil"
        , ctorArity = 0
        , ctorArgTypes = []
        , ctorReturnType = typeConApp [] "List" [TyVar (var "a" 0)]
        }

      -- Cons : a -> List a -> List a
      consCtor = Constructor
        { ctorName = qname [] "Cons"
        , ctorArity = 2
        , ctorArgTypes = [TyVar (var "a" 0), typeConApp [] "List" [TyVar (var "a" 0)]]
        , ctorReturnType = typeConApp [] "List" [TyVar (var "a" 0)]
        }

      db' = addConstructor db listTy nilCtor
      db'' = addConstructor db' listTy consCtor
  in db''

testListTypeDB :: Assertion
testListTypeDB =
  let db = mkListDB
      listTy = qname [] "List"
  in case queryConstructors db listTy of
       Right ctors -> do
         length ctors @?= 2
         [ctorArity c | c <- ctors] `shouldContain` [0, 2]
       Left err -> assertFailure $ "Query failed: " ++ prettyPatternError err
  where
    shouldContain xs ys = all (\y -> elem y xs) ys @?= True

testSimpleListPatterns :: Assertion
testSimpleListPatterns =
  let nilPat = PatConstr (qname [] "Nil") []
      consPat = PatConstr (qname [] "Cons") [PatVar (var "h" 0), PatVar (var "t" 1)]
  in do
    Right cp1 <- return $ compilePattern nilPat
    Right cp2 <- return $ compilePattern consPat

    cpOriginal cp1 @?= nilPat
    cpOriginal cp2 @?= consPat

    Map.size (cpBindings cp1) @?= 0
    Map.size (cpBindings cp2) @?= 2

testListPatternsExhaustive :: Assertion
testListPatternsExhaustive =
  let db = mkListDB
      listTy = typeConApp [] "List" [typeCon [] "Int"]
      nilPat = PatConstr (qname [] "Nil") []
      consPat = PatConstr (qname [] "Cons") [PatVar (var "h" 0), PatVar (var "t" 1)]
      patterns = [nilPat, consPat]
  in case checkExhaustive db listTy patterns Nothing of
       Right () -> return ()
       Left err -> assertFailure $ "Should be exhaustive: " ++ prettyExhaustiveError err

-- ============================================================================
-- PATTERN TYPE INFERENCE TESTS
-- ============================================================================

testInferVarType :: Assertion
testInferVarType =
  let ctx = PatternContext
        { pcType = typeCon [] "Int"
        , pcConstructors = emptyConstructorDB
        }
      pat = PatVar (var "x" 0)
  in case inferPatternType ctx pat of
       Right ty -> ty @?= typeCon [] "Int"
       Left err -> assertFailure $ "Type inference failed: " ++ prettyPatternError err

testInferConstrType :: Assertion
testInferConstrType =
  let ctx = PatternContext
        { pcType = typeConApp [] "List" [typeCon [] "Bool"]
        , pcConstructors = mkListDB
        }
      pat = PatConstr (qname [] "Nil") []
  in case inferPatternType ctx pat of
       Right ty -> ty @?= typeConApp [] "List" [typeCon [] "Bool"]
       Left err -> assertFailure $ "Type inference failed: " ++ prettyPatternError err

-- ============================================================================
-- ERROR HANDLING TESTS
-- ============================================================================

testUnknownConstructorError :: Assertion
testUnknownConstructorError =
  let db = emptyConstructorDB
      unknownTy = qname [] "Unknown"
  in case queryConstructors db unknownTy of
       Left (UnknownConstructor _) -> return ()
       Right _ -> assertFailure "Should fail"
       Left err -> assertFailure $ "Wrong error type: " ++ show err

testArityMismatchError :: Assertion
testArityMismatchError =
  let arity = 3
      expected = 2
  in case Left (ConstructorArityMismatch (qname [] "Cons") expected arity) of
       Left (ConstructorArityMismatch _ exp act) -> do
         exp @?= expected
         act @?= arity
       _ -> assertFailure "Error construction failed"

-- ============================================================================
-- PATTERN UTILITY TESTS
-- ============================================================================

testIsWildcardPattern :: Assertion
testIsWildcardPattern = do
  isWildcardPattern PatWildcard @?= True
  isWildcardPattern (PatVar (var "x" 0)) @?= True
  isWildcardPattern (PatConstr (qname [] "Nil") []) @?= False

testPatternConstructors :: Assertion
testPatternConstructors =
  let pat = PatConstr (qname [] "Cons")
              [PatVar (var "h" 0), PatConstr (qname [] "Nil") []]
      ctors = patternConstructors pat
  in do
    Set.member (qname [] "Cons") ctors @?= True
    Set.member (qname [] "Nil") ctors @?= True
    Set.size ctors @?= 2

testRedundantPatterns :: Assertion
testRedundantPatterns =
  let nilPat = PatConstr (qname [] "Nil") []
      consPat = PatConstr (qname [] "Cons") [PatVar (var "h" 0), PatVar (var "t" 1)]
      dupNil = PatConstr (qname [] "Nil") []
  in do
    isRedundant [nilPat] dupNil @?= True
    isRedundant [nilPat] consPat @?= False
    isRedundant [] nilPat @?= False
