{-|
Module      : Test.Surface.Lexer
Description : Unit tests for the lexer
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT
-}

module Test.Surface.Lexer (lexerTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Assertica.Surface.Lexer
import Assertica.Surface.AST (SourceLoc(..))

-- ============================================================================
-- LEXER TESTS
-- ============================================================================

lexerTests :: TestTree
lexerTests = testGroup "Lexer Tests"
  [ testGroup "Keywords"
      [ testCase "lex 'let'" $
          lexKeyword "let" KwLet
      , testCase "lex 'in'" $
          lexKeyword "in" KwIn
      , testCase "lex 'forall'" $
          lexKeyword "forall" KwForall
      , testCase "lex 'assert'" $
          lexKeyword "assert" KwAssert
      , testCase "lex 'proof'" $
          lexKeyword "proof" KwProof
      , testCase "lex 'data'" $
          lexKeyword "data" KwData
      , testCase "lex 'type'" $
          lexKeyword "type" KwType
      ]
  , testGroup "Identifiers"
      [ testCase "lex simple identifier" $
          lexIdent "x" (Ident "x")
      , testCase "lex longer identifier" $
          lexIdent "join" (Ident "join")
      , testCase "lex identifier with underscores" $
          lexIdent "my_var" (Ident "my_var")
      , testCase "lex qualified name" $
          lexIdent "Prelude.Bool" (QualIdent ["Prelude", "Bool"])
      ]
  , testGroup "Operators"
      [ testCase "lex '->' arrow" $
          lexOp "->" OpArrow
      , testCase "lex '=>' fat arrow" $
          lexOp "=>" OpFatArrow
      , testCase "lex ':=' colon equals" $
          lexOp ":=" OpColonEq
      , testCase "lex '==' equality" $
          lexOp "==" OpEq
      , testCase "lex '/=' not equal" $
          lexOp "/=" OpNeq
      , testCase "lex '+' plus" $
          lexOp "+" OpPlus
      , testCase "lex '-' minus" $
          lexOp "-" OpMinus
      , testCase "lex '*' multiply" $
          lexOp "*" OpMul
      , testCase "lex '/' divide" $
          lexOp "/" OpDiv
      , testCase "lex '/\\' and" $
          lexOp "/\\" OpAnd
      , testCase "lex '\\/' or" $
          lexOp "\\/" OpOr
      ]
  , testGroup "Literals"
      [ testCase "lex integer literal" $
          lexNum "42" (IntLiteral 42)
      , testCase "lex large integer" $
          lexNum "123456789" (IntLiteral 123456789)
      , testCase "lex string literal" $
          lexStr "\"hello\"" (StringLiteral "hello")
      , testCase "lex string with spaces" $
          lexStr "\"hello world\"" (StringLiteral "hello world")
      ]
  , testGroup "Delimiters"
      [ testCase "lex '('" $
          lexDelim "(" LParen
      , testCase "lex ')'" $
          lexDelim ")" RParen
      , testCase "lex '['" $
          lexDelim "[" LBracket
      , testCase "lex ']'" $
          lexDelim "]" RBracket
      , testCase "lex '{'" $
          lexDelim "{" LBrace
      , testCase "lex '}'" $
          lexDelim "}" RBrace
      , testCase "lex ','" $
          lexDelim "," Comma
      , testCase "lex ';'" $
          lexDelim ";" Semicolon
      ]
  , testGroup "Comments"
      [ testCase "skip line comment" $
          lexSkipLineComment "x -- comment\ny" [Ident "x", Ident "y"]
      , testCase "skip block comment" $
          lexSkipBlockComment "x {- comment -} y" [Ident "x", Ident "y"]
      , testCase "skip nested block comments" $
          lexSkipBlockComment "x {- a {- b -} c -} y" [Ident "x", Ident "y"]
      ]
  , testGroup "Complex programs"
      [ testCase "lex lambda expression" $
          lexProgram "\\x -> x + 1"
            [Backslash, Ident "x", OpArrow, Ident "x", OpPlus, IntLiteral 1]
      , testCase "lex assertion" $
          lexProgram "assert comm: forall x y. x == y"
            [KwAssert, Ident "comm", OpColon, KwForall, Ident "x", Ident "y", OpDot, Ident "x", OpEq, Ident "y"]
      , testCase "lex let binding" $
          lexProgram "let x = 42 in x"
            [KwLet, Ident "x", OpEq, IntLiteral 42, KwIn, Ident "x"]
      ]
  , testGroup "Error cases"
      [ testCase "unterminated string" $
          case lexer "\"hello" of
            Left _ -> return ()
            Right _ -> fail "Should fail on unterminated string"
      , testCase "unterminated block comment" $
          case lexer "x {- comment" of
            Left _ -> return ()
            Right _ -> fail "Should fail on unterminated block comment"
      ]
  ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Test lexing a single keyword.
-}
lexKeyword :: String -> TokenType -> Assertion
lexKeyword input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test lexing a single identifier.
-}
lexIdent :: String -> TokenType -> Assertion
lexIdent input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test lexing a single operator.
-}
lexOp :: String -> TokenType -> Assertion
lexOp input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test lexing a number.
-}
lexNum :: String -> TokenType -> Assertion
lexNum input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test lexing a string.
-}
lexStr :: String -> TokenType -> Assertion
lexStr input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test lexing a delimiter.
-}
lexDelim :: String -> TokenType -> Assertion
lexDelim input expected = do
  case lexer input of
    Right tokens ->
      case filter (\t -> tokenType t /= EOF) tokens of
        [Token tt _] -> tt @?= expected
        _ -> fail "Expected exactly one non-EOF token"
    Left err -> fail err

{-| Test skipping line comments.
-}
lexSkipLineComment :: String -> [TokenType] -> Assertion
lexSkipLineComment input expectedTypes = do
  case lexer input of
    Right tokens ->
      let types = map tokenType (filter (\t -> tokenType t /= EOF) tokens)
      in types @?= expectedTypes
    Left err -> fail err

{-| Test skipping block comments.
-}
lexSkipBlockComment :: String -> [TokenType] -> Assertion
lexSkipBlockComment input expectedTypes = do
  case lexer input of
    Right tokens ->
      let types = map tokenType (filter (\t -> tokenType t /= EOF) tokens)
      in types @?= expectedTypes
    Left err -> fail err

{-| Test lexing a complete program.
-}
lexProgram :: String -> [TokenType] -> Assertion
lexProgram input expectedTypes = do
  case lexer input of
    Right tokens ->
      let types = map tokenType (filter (\t -> tokenType t /= EOF) tokens)
      in types @?= expectedTypes
    Left err -> fail err
