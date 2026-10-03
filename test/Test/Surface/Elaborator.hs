{-|
Module      : Test.Surface.Elaborator
Description : Unit tests for the elaborator
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT
-}

module Test.Surface.Elaborator (elaboratorTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertEqual)
import Assertica.Surface.Elaborator
import Assertica.Surface.AST
import Assertica.Surface.Lexer (lexer)
import Assertica.Surface.Parser (parseTerm, parseType, parseProposition, parseAssertion)
import qualified Assertica.Core.AST as Core
import Data.Text (pack)

-- ============================================================================
-- ELABORATOR TESTS
-- ============================================================================

elaboratorTests :: TestTree
elaboratorTests = testGroup "Elaborator Tests"
  [ testGroup "Terms"
      [ testCase "elaborate variable" $
          elaborateTermTest "x" isVar
      , testCase "elaborate integer constant" $
          elaborateTermTest "42" isConstInt
      , testCase "elaborate application" $
          elaborateTermTest "f x" isApp
      , testCase "elaborate lambda" $
          elaborateTermTest "\\x -> x" isLam
      , testCase "elaborate let binding" $
          elaborateTermTest "let x = 42 in x" isLet
      ]
  , testGroup "Types"
      [ testCase "elaborate type variable" $
          elaborateTypeTest "a" isTyVar
      , testCase "elaborate function type" $
          elaborateTypeTest "Int -> String" isTyFun
      , testCase "elaborate type constant" $
          elaborateTypeTest "Int" isTyConst
      ]
  , testGroup "Propositions"
      [ testCase "elaborate equality" $
          elaboratePropositionTest "x == y" isEq
      , testCase "elaborate forall proposition" $
          elaboratePropositionTest "forall x. x == x" isForall
      ]
  , testGroup "Assertions"
      [ testCase "elaborate simple assertion" $
          elaborateAssertionTest "assert test: forall x. x == x"
      ]
  , testGroup "Name resolution"
      [ testCase "resolve unbound variable as global" $
          elaborateTermTest "x" isVar
      , testCase "resolve qualified name" $
          elaborateTermTest "Prelude.Bool" isConstr
      ]
  ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

dummyLoc :: SourceLoc
dummyLoc = SourceLoc "test.as" 1 1

{-| Elaborate a term and check it with a predicate.
-}
elaborateTermTest :: String -> (Core.Term -> Bool) -> Assertion
elaborateTermTest input predicate = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseTerm tokens of
        Left err -> fail (show err)
        Right term -> do
          case elaborateTerm term of
            Left err -> fail (show err)
            Right coreTerm ->
              if predicate coreTerm
              then return ()
              else fail ("Term does not match predicate: " ++ show coreTerm)

{-| Elaborate a type and check it with a predicate.
-}
elaborateTypeTest :: String -> (Core.Type -> Bool) -> Assertion
elaborateTypeTest input predicate = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseType tokens of
        Left err -> fail (show err)
        Right surfaceType -> do
          case elaborateType surfaceType of
            Left err -> fail (show err)
            Right coreType ->
              if predicate coreType
              then return ()
              else fail ("Type does not match predicate: " ++ show coreType)

{-| Elaborate a proposition and check it with a predicate.
-}
elaboratePropositionTest :: String -> (Core.Proposition -> Bool) -> Assertion
elaboratePropositionTest input predicate = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProposition tokens of
        Left err -> fail (show err)
        Right surfaceProp -> do
          case elaborateProposition surfaceProp of
            Left err -> fail (show err)
            Right coreProp ->
              if predicate coreProp
              then return ()
              else fail ("Proposition does not match predicate: " ++ show coreProp)

{-| Elaborate an assertion and check it succeeds.
-}
elaborateAssertionTest :: String -> Assertion
elaborateAssertionTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseAssertion tokens of
        Left err -> fail (show err)
        Right surfaceAssertion -> do
          case elaborateAssertion surfaceAssertion of
            Left err -> fail (show err)
            Right _assertion -> return ()

-- ============================================================================
-- PREDICATES FOR TERM CHECKING
-- ============================================================================

isVar :: Core.Term -> Bool
isVar Core.Var{} = True
isVar _ = False

isConstInt :: Core.Term -> Bool
isConstInt (Core.Const (Core.IntLit _)) = True
isConstInt _ = False

isApp :: Core.Term -> Bool
isApp Core.App{} = True
isApp _ = False

isLam :: Core.Term -> Bool
isLam Core.Lam{} = True
isLam _ = False

isLet :: Core.Term -> Bool
isLet Core.Let{} = True
isLet _ = False

isConstr :: Core.Term -> Bool
isConstr Core.Constr{} = True
isConstr _ = False

-- ============================================================================
-- PREDICATES FOR TYPE CHECKING
-- ============================================================================

isTyVar :: Core.Type -> Bool
isTyVar Core.TyVar{} = True
isTyVar _ = False

isTyFun :: Core.Type -> Bool
isTyFun Core.TyFun{} = True
isTyFun _ = False

isTyConst :: Core.Type -> Bool
isTyConst Core.TyConst{} = True
isTyConst _ = False

-- ============================================================================
-- PREDICATES FOR PROPOSITION CHECKING
-- ============================================================================

isEq :: Core.Proposition -> Bool
isEq Core.Eq{} = True
isEq _ = False

isForall :: Core.Proposition -> Bool
isForall Core.Forall'{} = True
isForall _ = False
