{-|
Module      : Assertica.Surface.Lexer
Description : Lexer for Haskell-like surface syntax
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Tokenizes Haskell-like source code into a stream of tokens,
preserving source location information for error reporting.

Token categories:
- Keywords: let, in, forall, assert, proof, data, type, class, instance
- Identifiers: simple names and qualified names (Mod.name)
- Operators: +, -, *, /, ==, /=, ->, =>, :=, etc.
- Literals: integers, strings, booleans
- Delimiters: ( ) [ ] { } , ; :
- Comments: -- single-line, {- multi-line -}
-}

module Assertica.Surface.Lexer
  ( Token (..)
  , TokenType (..)
  , Lexer.SourceLoc (..)
  , lexer
  , lexerWithLoc
  , prettyToken
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import qualified Assertica.Surface.AST as Lexer (SourceLoc (..))
import Assertica.Surface.AST (SourceLoc (..))

-- ============================================================================
-- TOKEN TYPES
-- ============================================================================

{-| Token type classification.
-}
data TokenType
  -- Keywords
  = KwLet | KwIn | KwForall | KwAssert | KwProof
  | KwData | KwType | KwClass | KwInstance | KwWhere
  | KwCase | KwOf | KwIf | KwThen | KwElse
  | KwBy | KwRefl | KwSymm | KwTrans
  | KwTrue | KwFalse

  -- Identifiers and literals
  | Ident Text                    -- ^ Simple identifier
  | QualIdent [Text]              -- ^ Qualified identifier (Mod.name)
  | IntLiteral Integer
  | StringLiteral String

  -- Operators
  | OpArrow                        -- ^ ->
  | OpFatArrow                     -- ^ =>
  | OpColonEq                      -- ^ :=
  | OpEq                           -- ^ ==
  | OpNeq                          -- ^ /= or !=
  | OpLt | OpLe | OpGt | OpGe
  | OpPlus | OpMinus | OpMul | OpDiv | OpMod
  | OpAnd | OpOr                   -- ^ /\ and \/
  | OpNot
  | OpConcat                       -- ^ ++
  | OpColon                        -- ^ :
  | OpDot                          -- ^ .

  -- Delimiters
  | LParen | RParen
  | LBracket | RBracket
  | LBrace | RBrace
  | Comma | Semicolon | Pipe

  -- Special
  | Underscore
  | Backslash                      -- ^ \
  | EOF
  deriving (Eq, Show)

{-| A token with source location.
-}
data Token = Token
  { tokenType :: TokenType
  , tokenLoc  :: SourceLoc
  }
  deriving (Eq, Show)

-- ============================================================================
-- LEXER STATE AND HELPERS
-- ============================================================================

{-| Lexer state tracking position.
-}
data LexState = LexState
  { lsInput  :: String
  , lsPos    :: Int
  , lsLine   :: Int
  , lsCol    :: Int
  , lsFile   :: String
  }
  deriving (Show)

{-| Initialize lexer state.
-}
initLexState :: String -> String -> LexState
initLexState file input = LexState input 0 1 1 file

{-| Get current source location.
-}
currentLoc :: LexState -> SourceLoc
currentLoc ls = SourceLoc (lsFile ls) (lsLine ls) (lsCol ls)

{-| Peek at the current character without consuming it.
-}
peek :: LexState -> Maybe Char
peek ls =
  if lsPos ls < length (lsInput ls)
  then Just (lsInput ls !! lsPos ls)
  else Nothing

{-| Consume a character and update position tracking.
-}
consume :: LexState -> LexState
consume ls = case peek ls of
  Nothing -> ls
  Just '\n' -> ls { lsPos = lsPos ls + 1, lsLine = lsLine ls + 1, lsCol = 1 }
  Just _ -> ls { lsPos = lsPos ls + 1, lsCol = lsCol ls + 1 }

{-| Peek n characters ahead.
-}
peekN :: Int -> LexState -> String
peekN n ls = take n (drop (lsPos ls) (lsInput ls))

-- ============================================================================
-- LEXER
-- ============================================================================

{-| Lex source code into a stream of tokens.
Returns either an error message or a list of tokens.
-}
lexer :: String -> Either String [Token]
lexer input = lexerWithLoc "<input>" input

{-| Lex source code with explicit file name.
-}
lexerWithLoc :: String -> String -> Either String [Token]
lexerWithLoc file input = do
  tokens <- lexTokens (initLexState file input)
  return tokens

{-| Lexically analyze tokens.
-}
lexTokens :: LexState -> Either String [Token]
lexTokens ls = case peek ls of
  Nothing -> Right [Token EOF (currentLoc ls)]
  Just c
    | isSpace c -> lexTokens (skipWhitespace ls)
    | c == '-' && peekN 2 ls == "--" -> lexTokens (skipLineComment ls)
    | c == '{' && peekN 2 ls == "{-" -> do
        ls' <- skipBlockComment ls
        lexTokens ls'
    | otherwise -> do
        (tok, ls') <- lexToken ls
        rest <- lexTokens ls'
        return (tok : rest)

{-| Skip whitespace.
-}
skipWhitespace :: LexState -> LexState
skipWhitespace ls = case peek ls of
  Just c | isSpace c -> skipWhitespace (consume ls)
  _ -> ls

{-| Skip single-line comment (-- ...)
-}
skipLineComment :: LexState -> LexState
skipLineComment ls = case peek ls of
  Just '\n' -> consume ls
  Just _ -> skipLineComment (consume ls)
  Nothing -> ls

{-| Skip multi-line comment ({- ... -})
Handles nested comments.
-}
skipBlockComment :: LexState -> Either String LexState
skipBlockComment ls0 = go 1 (consume (consume ls0))
  where
    go :: Int -> LexState -> Either String LexState
    go depth ls
      | depth == 0 = Right ls
      | otherwise = case peekN 2 ls of
          "{-" -> go (depth + 1) (consume (consume ls))
          "-}" -> go (depth - 1) (consume (consume ls))
          _ -> case peek ls of
            Nothing -> Left "Unterminated block comment"
            Just _ -> go depth (consume ls)

{-| Lex a single token.
-}
lexToken :: LexState -> Either String (Token, LexState)
lexToken ls =
  let loc = currentLoc ls
  in case peek ls of
    Nothing -> Right (Token EOF loc, ls)
    Just '(' -> Right (Token LParen loc, consume ls)
    Just ')' -> Right (Token RParen loc, consume ls)
    Just '[' -> Right (Token LBracket loc, consume ls)
    Just ']' -> Right (Token RBracket loc, consume ls)
    Just '{' -> Right (Token LBrace loc, consume ls)
    Just '}' -> Right (Token RBrace loc, consume ls)
    Just ',' -> Right (Token Comma loc, consume ls)
    Just ';' -> Right (Token Semicolon loc, consume ls)
    Just '|' -> Right (Token Pipe loc, consume ls)
    Just '\\' -> Right (Token Backslash loc, consume ls)
    Just '_' ->
      if isIdNext (peekN 2 ls)
      then lexIdent ls
      else Right (Token Underscore loc, consume ls)
    Just '"' -> do
      (s, ls') <- lexString ls
      Right (Token (StringLiteral s) loc, ls')
    Just c
      | isDigit c -> do
          (n, ls') <- lexNumber ls
          Right (Token (IntLiteral n) loc, ls')
      | isAlpha c || c == '_' -> lexIdent ls
      | otherwise -> lexOperator ls

{-| Check if the next character would continue an identifier.
-}
isIdNext :: String -> Bool
isIdNext (c:_) = isAlphaNum c || c == '_' || c == '\''
isIdNext [] = False

{-| Lex an identifier or keyword.
-}
lexIdent :: LexState -> Either String (Token, LexState)
lexIdent ls =
  let loc = currentLoc ls
      (ident, ls') = span (\c -> isAlphaNum c || c == '_' || c == '\'') (drop (lsPos ls) (lsInput ls))
      ls'' = ls { lsPos = lsPos ls + length ident, lsCol = lsCol ls + length ident }
      txt = T.pack ident
  in case T.unpack txt of
    "let"     -> Right (Token KwLet loc, ls'')
    "in"      -> Right (Token KwIn loc, ls'')
    "forall"  -> Right (Token KwForall loc, ls'')
    "assert"  -> Right (Token KwAssert loc, ls'')
    "proof"   -> Right (Token KwProof loc, ls'')
    "data"    -> Right (Token KwData loc, ls'')
    "type"    -> Right (Token KwType loc, ls'')
    "class"   -> Right (Token KwClass loc, ls'')
    "instance"-> Right (Token KwInstance loc, ls'')
    "where"   -> Right (Token KwWhere loc, ls'')
    "case"    -> Right (Token KwCase loc, ls'')
    "of"      -> Right (Token KwOf loc, ls'')
    "if"      -> Right (Token KwIf loc, ls'')
    "then"    -> Right (Token KwThen loc, ls'')
    "else"    -> Right (Token KwElse loc, ls'')
    "by"      -> Right (Token KwBy loc, ls'')
    "by_refl" -> Right (Token KwRefl loc, ls'')
    "refl"    -> Right (Token KwRefl loc, ls'')
    "by_symm" -> Right (Token KwSymm loc, ls'')
    "symm"    -> Right (Token KwSymm loc, ls'')
    "by_trans"-> Right (Token KwTrans loc, ls'')
    "trans"   -> Right (Token KwTrans loc, ls'')
    "True"    -> Right (Token KwTrue loc, ls'')
    "False"   -> Right (Token KwFalse loc, ls'')
    _         ->
      -- Check for qualified identifiers (Mod.name)
      if '.' `elem` ident
      then
        let parts = T.splitOn (T.pack ".") txt
        in Right (Token (QualIdent (map T.unpack parts)) loc, ls'')
      else
        Right (Token (Ident txt) loc, ls'')

{-| Lex a numeric literal.
-}
lexNumber :: LexState -> Either String (Integer, LexState)
lexNumber ls =
  let numStr = takeWhile isDigit (drop (lsPos ls) (lsInput ls))
      num = read numStr :: Integer
      ls' = ls { lsPos = lsPos ls + length numStr, lsCol = lsCol ls + length numStr }
  in Right (num, ls')

{-| Lex a string literal.
-}
lexString :: LexState -> Either String (String, LexState)
lexString ls0 =
  let ls = consume ls0  -- skip opening quote
      go acc ls' = case peek ls' of
        Nothing -> Left "Unterminated string"
        Just '"' -> Right (reverse acc, consume ls')
        Just '\\' ->
          case peekN 2 ls' of
            "\\n" -> go ('\n' : acc) (consume (consume ls'))
            "\\t" -> go ('\t' : acc) (consume (consume ls'))
            "\\\"" -> go ('"' : acc) (consume (consume ls'))
            "\\\\" -> go ('\\' : acc) (consume (consume ls'))
            _ -> go (head (drop 1 (peekN 2 ls')) : acc) (consume (consume ls'))
        Just c -> go (c : acc) (consume ls')
  in go "" ls

{-| Lex an operator.
-}
lexOperator :: LexState -> Either String (Token, LexState)
lexOperator ls =
  let loc = currentLoc ls
  in case peekN 3 ls of
    (':' : '=' : _) -> Right (Token OpColonEq loc, consume (consume ls))
    ('=' : '>' : _) -> Right (Token OpFatArrow loc, consume (consume ls))
    ('-' : '>' : _) -> Right (Token OpArrow loc, consume (consume ls))
    ('=' : '=' : _) -> Right (Token OpEq loc, consume (consume ls))
    ('/' : '=' : _) -> Right (Token OpNeq loc, consume (consume ls))
    ('!' : '=' : _) -> Right (Token OpNeq loc, consume (consume ls))
    ('<' : '=' : _) -> Right (Token OpLe loc, consume (consume ls))
    ('>' : '=' : _) -> Right (Token OpGe loc, consume (consume ls))
    ('/' : '\\' : _) -> Right (Token OpAnd loc, consume (consume ls))
    ('\\' : '/' : _) -> Right (Token OpOr loc, consume (consume ls))
    ('+' : '+' : _) -> Right (Token OpConcat loc, consume (consume ls))
    _ -> case peek ls of
      Just '+' -> Right (Token OpPlus loc, consume ls)
      Just '-' -> Right (Token OpMinus loc, consume ls)
      Just '*' -> Right (Token OpMul loc, consume ls)
      Just '/' -> Right (Token OpDiv loc, consume ls)
      Just '%' -> Right (Token OpMod loc, consume ls)
      Just '<' -> Right (Token OpLt loc, consume ls)
      Just '>' -> Right (Token OpGt loc, consume ls)
      Just ':' -> Right (Token OpColon loc, consume ls)
      Just '.' -> Right (Token OpDot loc, consume ls)
      Just c -> Left $ "Unexpected character: " ++ [c]
      Nothing -> Right (Token EOF loc, ls)

-- ============================================================================
-- PRETTY PRINTING
-- ============================================================================

{-| Pretty-print a token for debugging.
-}
prettyToken :: Token -> String
prettyToken (Token tt loc) =
  let loc_str = show (lsLine loc) ++ ":" ++ show (lsCol loc)
  in loc_str ++ " " ++ prettyTokenType tt

{-| Pretty-print a token type.
-}
prettyTokenType :: TokenType -> String
prettyTokenType = \case
  KwLet -> "KwLet"
  KwIn -> "KwIn"
  KwForall -> "KwForall"
  KwAssert -> "KwAssert"
  KwProof -> "KwProof"
  KwData -> "KwData"
  KwType -> "KwType"
  KwClass -> "KwClass"
  KwInstance -> "KwInstance"
  KwWhere -> "KwWhere"
  KwCase -> "KwCase"
  KwOf -> "KwOf"
  KwIf -> "KwIf"
  KwThen -> "KwThen"
  KwElse -> "KwElse"
  KwBy -> "KwBy"
  KwRefl -> "KwRefl"
  KwSymm -> "KwSymm"
  KwTrans -> "KwTrans"
  KwTrue -> "KwTrue"
  KwFalse -> "KwFalse"
  Ident x -> "Ident(" ++ T.unpack x ++ ")"
  QualIdent xs -> "QualIdent(" ++ intercalate "." xs ++ ")"
  IntLiteral n -> "IntLit(" ++ show n ++ ")"
  StringLiteral s -> "StringLit(" ++ show s ++ ")"
  OpArrow -> "->"
  OpFatArrow -> "=>"
  OpColonEq -> ":="
  OpEq -> "=="
  OpNeq -> "/="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpPlus -> "+"
  OpMinus -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAnd -> "/\\"
  OpOr -> "\\/"
  OpNot -> "not"
  OpConcat -> "++"
  OpColon -> ":"
  OpDot -> "."
  LParen -> "("
  RParen -> ")"
  LBracket -> "["
  RBracket -> "]"
  LBrace -> "{"
  RBrace -> "}"
  Comma -> ","
  Semicolon -> ";"
  Pipe -> "|"
  Underscore -> "_"
  Backslash -> "\\"
  EOF -> "EOF"
