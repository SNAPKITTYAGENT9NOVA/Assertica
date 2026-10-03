{-|
Module      : Test.Surface.Parser
Description : Unit tests for the parser
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT
-}

module Test.Surface.Parser (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Assertica.Surface.Parser
import Assertica.Surface.AST
import Assertica.Surface.Lexer (lexer)
import Data.Text (pack)

-- ============================================================================
-- PARSER TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Surface.Parser"
  [ testGroup "Terms"
      [ testCase "parse variable" $
          parseTermTest "x" (STVar (pack "x"))
      , testCase "parse integer literal" $
          parseTermTest "42" (STConst (IntLit 42))
      , testCase "parse boolean true" $
          parseTermTest "True" (STConst (BoolLit True))
      , testCase "parse boolean false" $
          parseTermTest "False" (STConst (BoolLit False))
      , testCase "parse string literal" $
          parseTermTest "\"hello\"" (STConst (StringLit "hello"))
      , testCase "parse application" $
          parseTermTest "f x" (STApp (Located (STVar (pack "f")) dummyLoc) [Located (STVar (pack "x")) dummyLoc])
      , testCase "parse multi-arg application" $
          parseTermTest "f x y z"
            (STApp (Located (STVar (pack "f")) dummyLoc)
              [Located (STVar (pack "x")) dummyLoc, Located (STVar (pack "y")) dummyLoc, Located (STVar (pack "z")) dummyLoc])
      , testCase "parse lambda" $
          parseTermLamTest "\\x -> x"
      , testCase "parse let binding" $
          parseTermLetTest "let x = 42 in x"
      , testCase "parse infix expression" $
          parseTermInfixTest "x + y"
      ]
  , testGroup "Types"
      [ testCase "parse type variable" $
          parseTypeTest "a" (STyVar (pack "a"))
      , testCase "parse type constant" $
          parseTypeTest "Int" (STyConst [pack "Int"] [])
      , testCase "parse function type" $
          parseTypeTest "Int -> String"
            (STyFun (STyConst [pack "Int"] []) (STyConst [pack "String"] []))
      , testCase "parse forall type" $
          parseTypeForallTest "forall a. a -> a"
      ]
  , testGroup "Propositions"
      [ testCase "parse equality" $
          parsePropositionTest "x == y" SPEq
      , testCase "parse forall proposition" $
          parsePropositionForallTest "forall x. x == x"
      , testCase "parse exists proposition" $
          parsePropositionExistsTest "exists x. x == x"
      ]
  , testGroup "Proofs"
      [ testCase "parse refl proof" $
          parseProofTest "refl" SPRefl
      , testCase "parse by_refl proof" $
          parseProofTest "by_refl" (SPByName (pack "by_refl"))
      ]
  , testGroup "Complete programs"
      [ testCase "parse assertion" $
          parseAssertionCompleteTest
      , testCase "parse proof" $
          parseProofCompleteTest
      ]
  ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

dummyLoc :: SourceLoc
dummyLoc = SourceLoc "test.as" 1 1

{-| Parse a term from string and extract the core term.
-}
parseTermTest :: String -> SurfaceTerm -> Assertion
parseTermTest input expected = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseTerm tokens of
        Left err -> fail (show err)
        Right (Located term _) -> term @?= expected

{-| Test parsing a lambda expression (just check it succeeds).
-}
parseTermLamTest :: String -> Assertion
parseTermLamTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseTerm tokens of
        Left err -> fail (show err)
        Right (Located STLam{} _) -> return ()
        Right (Located term _) -> fail ("Expected lambda, got: " ++ show term)

{-| Test parsing a let binding (just check it succeeds).
-}
parseTermLetTest :: String -> Assertion
parseTermLetTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseTerm tokens of
        Left err -> fail (show err)
        Right (Located STLet{} _) -> return ()
        Right (Located term _) -> fail ("Expected let, got: " ++ show term)

{-| Test parsing an infix expression.
-}
parseTermInfixTest :: String -> Assertion
parseTermInfixTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseTerm tokens of
        Left err -> fail (show err)
        Right (Located STInfix{} _) -> return ()
        Right (Located term _) -> fail ("Expected infix, got: " ++ show term)

{-| Parse a type from string.
-}
parseTypeTest :: String -> SurfaceType -> Assertion
parseTypeTest input expected = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseType tokens of
        Left err -> fail (show err)
        Right ty -> ty @?= expected

{-| Test parsing a forall type.
-}
parseTypeForallTest :: String -> Assertion
parseTypeForallTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseType tokens of
        Left err -> fail (show err)
        Right STyForall{} -> return ()
        Right ty -> fail ("Expected forall type, got: " ++ show ty)

{-| Parse a proposition from string.
-}
parsePropositionTest :: String -> (SurfaceProposition -> Bool) -> Assertion
parsePropositionTest input predicate = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProposition tokens of
        Left err -> fail (show err)
        Right (Located prop _) ->
          if predicate prop
          then return ()
          else fail ("Proposition does not match predicate: " ++ show prop)

{-| Test parsing a forall proposition.
-}
parsePropositionForallTest :: String -> Assertion
parsePropositionForallTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProposition tokens of
        Left err -> fail (show err)
        Right (Located SPForall{} _) -> return ()
        Right (Located prop _) -> fail ("Expected forall, got: " ++ show prop)

{-| Test parsing an exists proposition.
-}
parsePropositionExistsTest :: String -> Assertion
parsePropositionExistsTest input = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProposition tokens of
        Left err -> fail (show err)
        Right (Located SPExists{} _) -> return ()
        Right (Located prop _) -> fail ("Expected exists, got: " ++ show prop)

{-| Parse a proof from string.
-}
parseProofTest :: String -> SurfaceProof -> Assertion
parseProofTest input expected = do
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProof tokens of
        Left err -> fail (show err)
        Right (Located proof _) -> proof @?= expected

{-| Test parsing a complete assertion.
-}
parseAssertionCompleteTest :: Assertion
parseAssertionCompleteTest = do
  let input = "assert commutativity: forall x y. x == y"
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseAssertion tokens of
        Left err -> fail (show err)
        Right SurfaceAssertion{} -> return ()

{-| Test parsing a complete proof.
-}
parseProofCompleteTest :: Assertion
parseProofCompleteTest = do
  let input = "proof my_proof : forall x y. x == y := refl"
  case lexer input of
    Left err -> fail err
    Right tokens -> do
      case parseProof tokens of
        Left err -> fail (show err)
        Right (Located proof _) ->
          case proof of
            SPRefl -> return ()
            _ -> fail "Expected refl proof"
