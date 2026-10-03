{-|
Module      : Assertica.Surface.Parser
Description : Parser for Haskell-like surface syntax
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Recursive descent parser that converts a token stream into the surface AST.
Implements operator precedence and handles syntactic sugar expansion.

Parser structure:
- Propositions: forall, equality, logical connectives
- Terms: lambda, application, let bindings, case expressions
- Types: function types, universally quantified types
-}

module Assertica.Surface.Parser
  ( -- * Main parsing functions
    parseProgram
  , parseTerm
  , parseType
  , parseProposition
  , parseAssertion
  , parseProof
  , Parser
  , ParserState (..)

    -- * Error types
  , ParseError (..)
  ) where

import Assertica.Surface.AST
import Assertica.Surface.Lexer (Token (..), TokenType (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (when)

-- ============================================================================
-- PARSER STATE AND MONAD
-- ============================================================================

{-| Parser state.
-}
data ParserState = ParserState
  { psTokens :: [Token]
  , psPos    :: Int
  }
  deriving (Show)

{-| Parser result: either an error or (value, new_state).
-}
type Parser a = ParserState -> Either ParseError (a, ParserState)

{-| Parse error with location information.
-}
data ParseError = ParseError
  { peMessage :: String
  , peLoc     :: Maybe SourceLoc
  }
  deriving (Eq, Show)

-- ============================================================================
-- PARSER PRIMITIVES
-- ============================================================================

{-| Run a parser on a token stream.
-}
runParser :: [Token] -> Parser a -> Either ParseError a
runParser tokens p = do
  (a, _) <- p (ParserState tokens 0)
  return a

{-| Return a pure value.
-}
return' :: a -> Parser a
return' a ps = Right (a, ps)

{-| Bind operation for the parser monad.
-}
bind :: Parser a -> (a -> Parser b) -> Parser b
bind pa f ps = do
  (a, ps') <- pa ps
  f a ps'

{-| Fail with an error message.
-}
fail' :: String -> Parser a
fail' msg ps =
  let loc = if psPos ps < length (psTokens ps)
            then Just (tokenLoc (psTokens ps !! psPos ps))
            else Nothing
  in Left (ParseError msg loc)

{-| Peek at the current token.
-}
peek :: Parser Token
peek ps =
  if psPos ps < length (psTokens ps)
  then Right (psTokens ps !! psPos ps, ps)
  else Left (ParseError "Unexpected end of input" Nothing)

{-| Consume the current token.
-}
consume :: Parser Token
consume ps =
  if psPos ps < length (psTokens ps)
  then Right (psTokens ps !! psPos ps, ps { psPos = psPos ps + 1 })
  else Left (ParseError "Unexpected end of input" Nothing)

{-| Match a specific token type.
-}
match :: TokenType -> Parser Token
match tt ps = do
  tok <- peek ps
  if tokenType tok == tt
    then consume ps
    else fail' ("Expected " ++ show tt ++ " but got " ++ show (tokenType tok)) ps

{-| Try to match an optional token.
-}
optional :: TokenType -> Parser Bool
optional tt ps =
  case peek ps of
    Right (tok, _) -> Right (tokenType tok == tt, ps)
    Left _ -> Right (False, ps)

{-| Try a parser, return Nothing if it fails.
-}
try :: Parser a -> Parser (Maybe a)
try p ps = case p ps of
  Right (a, ps') -> Right (Just a, ps')
  Left _ -> Right (Nothing, ps)

{-| Alternative: try first parser, if it fails try second.
-}
alt :: Parser a -> Parser a -> Parser a
alt p1 p2 ps = case p1 ps of
  Right result -> Right result
  Left _ -> p2 ps

{-| Sequence two parsers, return value of second.
-}
then' :: Parser a -> Parser b -> Parser b
then' p1 p2 ps = do
  (_, ps') <- p1 ps
  p2 ps'

-- ============================================================================
-- MAIN ENTRY POINTS
-- ============================================================================

{-| Parse a complete program (sequence of declarations).
-}
parseProgram :: [Token] -> Either ParseError [SurfaceDecl]
parseProgram tokens = runParser tokens parseDecls

{-| Parse a single term.
-}
parseTerm :: [Token] -> Either ParseError (Located SurfaceTerm)
parseTerm tokens = runParser tokens parseTerm'

{-| Parse a type.
-}
parseType :: [Token] -> Either ParseError SurfaceType
parseType tokens = runParser tokens parseType'

{-| Parse a proposition.
-}
parseProposition :: [Token] -> Either ParseError (Located SurfaceProposition)
parseProposition tokens = runParser tokens parseProposition'

{-| Parse an assertion.
-}
parseAssertion :: [Token] -> Either ParseError SurfaceAssertion
parseAssertion tokens = runParser tokens parseAssertion'

{-| Parse a proof.
-}
parseProof :: [Token] -> Either ParseError (Located SurfaceProof)
parseProof tokens = runParser tokens parseProof'

-- ============================================================================
-- DECLARATION PARSING
-- ============================================================================

{-| Parse a sequence of declarations.
-}
parseDecls :: Parser [SurfaceDecl]
parseDecls ps = do
  (d, ps') <- parseDecl ps
  case tokenType <$> peek ps' of
    Right EOF -> return [d] ps'
    _ -> do
      (ds, ps'') <- parseDecls ps'
      return (d : ds) ps''

{-| Parse a single declaration.
-}
parseDecl :: Parser SurfaceDecl
parseDecl = parseAssertionDecl `alt` parseProofDecl

{-| Parse an assertion declaration.
-}
parseAssertionDecl :: Parser SurfaceDecl
parseAssertionDecl ps = do
  _ <- match KwAssert ps
  (nameToken, ps1) <- peek ps
  name_loc <- case tokenType nameToken of
    Ident x -> return (Located x (tokenLoc nameToken), ps1 { psPos = psPos ps1 + 1 })
    _ -> fail' "Expected identifier after 'assert'" ps
  (name, ps2) <- name_loc
  _ <- match OpColon ps2
  (prop, ps3) <- parseProposition' ps2
  proof_opt <- try (match KwBy ps3) ps3
  (proof, ps4) <- case proof_opt of
    Just _ -> do
      (p, ps') <- parseProof' ps3
      return (Just p, ps')
    Nothing -> return (Nothing, ps3)
  let assertion = SurfaceAssertion (Located name (tokenLoc nameToken)) prop proof
  return (SDeclAssertion assertion, ps4)

{-| Parse a proof declaration.
-}
parseProofDecl :: Parser SurfaceDecl
parseProofDecl ps = do
  _ <- match KwProof ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'proof'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  _ <- match OpColon ps2
  (prop, ps3) <- parsePropositionRaw ps2
  _ <- match OpColonEq ps3
  (proof, ps4) <- parseProof' ps3
  return (SDeclProof name prop proof, ps4)

-- ============================================================================
-- TERM PARSING
-- ============================================================================

{-| Parse a term (main entry point).
-}
parseTerm' :: Parser (Located SurfaceTerm)
parseTerm' = parseTermPrec 0

{-| Parse a term with precedence (left-associative operators).
-}
parseTermPrec :: Int -> Parser (Located SurfaceTerm)
parseTermPrec minPrec ps = do
  (left, ps') <- parseAtomTerm ps
  parseTermRest minPrec left ps'

{-| Continue parsing term operators.
-}
parseTermRest :: Int -> Located SurfaceTerm -> Parser (Located SurfaceTerm)
parseTermRest minPrec left ps =
  case peek ps of
    Right (tok, _)
      | isInfixOp (tokenType tok) ->
          let prec = precedence (tokenType tok)
          in if prec >= minPrec
             then do
               (op, ps1) <- consume ps
               (right, ps2) <- parseTermPrec (prec + 1) ps1
               let result = Located (STInfix left (identFromOp (tokenType op)) right) (locLoc left)
               parseTermRest minPrec result ps2
             else return' left ps
      | otherwise -> return' left ps
    Left _ -> return' left ps

{-| Check if a token is an infix operator.
-}
isInfixOp :: TokenType -> Bool
isInfixOp = \case
  OpPlus -> True
  OpMinus -> True
  OpMul -> True
  OpDiv -> True
  OpMod -> True
  OpEq -> True
  OpNeq -> True
  OpLt -> True
  OpLe -> True
  OpGt -> True
  OpGe -> True
  OpAnd -> True
  OpOr -> True
  _ -> False

{-| Get operator precedence (higher = tighter binding).
-}
precedence :: TokenType -> Int
precedence = \case
  OpMul -> 7
  OpDiv -> 7
  OpMod -> 7
  OpPlus -> 6
  OpMinus -> 6
  OpEq -> 4
  OpNeq -> 4
  OpLt -> 4
  OpLe -> 4
  OpGt -> 4
  OpGe -> 4
  OpAnd -> 3
  OpOr -> 2
  _ -> 0

{-| Convert operator token to identifier.
-}
identFromOp :: TokenType -> Text
identFromOp = \case
  OpPlus -> T.pack "+"
  OpMinus -> T.pack "-"
  OpMul -> T.pack "*"
  OpDiv -> T.pack "/"
  OpMod -> T.pack "%"
  OpEq -> T.pack "=="
  OpNeq -> T.pack "/="
  OpLt -> T.pack "<"
  OpLe -> T.pack "<="
  OpGt -> T.pack ">"
  OpGe -> T.pack ">="
  OpAnd -> T.pack "/\\"
  OpOr -> T.pack "\\/"
  _ -> T.pack "op"

{-| Parse an atomic term (no operators).
-}
parseAtomTerm :: Parser (Located SurfaceTerm)
parseAtomTerm = parseLamTerm `alt` parseLetTerm `alt` parseCaseTerm
                `alt` parseForallTerm `alt` parseBasicTerm

{-| Parse a lambda abstraction.
-}
parseLamTerm :: Parser (Located SurfaceTerm)
parseLamTerm ps = do
  tok1 <- consume ps
  when (tokenType tok1 /= Backslash) $ fail' "Expected \\" ps
  (vars, ps1) <- parseIdentList ps { psPos = psPos ps + 1 }
  ty_opt <- try (match OpColon ps1) ps1
  (mty, ps2) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps1 { psPos = psPos ps1 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps1)
  _ <- match OpArrow ps2
  (body, ps3) <- parseTerm' ps2 { psPos = psPos ps2 + 1 }
  return (Located (STLam vars mty body) (locLoc body), ps3)

{-| Parse a let binding.
-}
parseLetTerm :: Parser (Located SurfaceTerm)
parseLetTerm ps = do
  _ <- match KwLet ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'let'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  ty_opt <- try (match OpColon ps2) ps2
  (mty, ps3) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps2 { psPos = psPos ps2 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match OpEq ps3
  (e1, ps4) <- parseTerm' ps3 { psPos = psPos ps3 + 1 }
  _ <- match KwIn ps4
  (e2, ps5) <- parseTerm' ps4 { psPos = psPos ps4 + 1 }
  let loc = tokenLoc nameToken
  return (Located (STLet name mty e1 e2) loc, ps5)

{-| Parse a case expression.
-}
parseCaseTerm :: Parser (Located SurfaceTerm)
parseCaseTerm ps = do
  tok1 <- consume ps
  when (tokenType tok1 /= KwCase) $ fail' "Expected 'case'" ps
  (e, ps1) <- parseTerm' ps { psPos = psPos ps + 1 }
  _ <- match KwOf ps1
  _ <- match LBrace ps1 { psPos = psPos ps1 + 1 }
  (clauses, ps2) <- parseClauseList ps1 { psPos = psPos ps1 + 2 }
  def_opt <- try (match Pipe ps2) ps2
  (mdef, ps3) <- case def_opt of
    Just _ -> do
      _ <- match Underscore ps2 { psPos = psPos ps2 + 1 }
      _ <- match OpArrow ps2 { psPos = psPos ps2 + 2 }
      (d, ps') <- parseTerm' ps2 { psPos = psPos ps2 + 3 }
      return (Just d, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match RBrace ps3
  let loc = locLoc e
  return (Located (STCase e clauses mdef) loc, ps3 { psPos = psPos ps3 + 1 })

{-| Parse a forall term.
-}
parseForallTerm :: Parser (Located SurfaceTerm)
parseForallTerm ps = do
  _ <- match KwForall ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'forall'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  ty_opt <- try (match OpColon ps2) ps2
  (mty, ps3) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps2 { psPos = psPos ps2 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match OpDot ps3
  (body, ps4) <- parseTerm' ps3 { psPos = psPos ps3 + 1 }
  let loc = tokenLoc nameToken
  return (Located (STForall name mty body) loc, ps4)

{-| Parse a basic term (variable, constant, application, tuple, etc.).
-}
parseBasicTerm :: Parser (Located SurfaceTerm)
parseBasicTerm ps = do
  tok <- consume ps
  let loc = tokenLoc tok
  case tokenType tok of
    Ident x -> do
      (args, ps') <- parseAppArgs ps { psPos = psPos ps + 1 }
      if null args
        then return (Located (STVar x) loc, ps')
        else return (Located (STApp (Located (STVar x) loc) args) loc, ps')
    QualIdent xs -> do
      let qn = map T.pack xs
      (args, ps') <- parseAppArgs ps { psPos = psPos ps + 1 }
      if null args
        then return (Located (STConstr qn []) loc, ps')
        else return (Located (STApp (Located (STConstr qn []) loc) args) loc, ps')
    IntLiteral n ->
      return (Located (STConst (IntLit n)) loc, ps { psPos = psPos ps + 1 })
    StringLiteral s ->
      return (Located (STConst (StringLit s)) loc, ps { psPos = psPos ps + 1 })
    KwTrue ->
      return (Located (STConst (BoolLit True)) loc, ps { psPos = psPos ps + 1 })
    KwFalse ->
      return (Located (STConst (BoolLit False)) loc, ps { psPos = psPos ps + 1 })
    LParen -> do
      (e, ps1) <- parseTerm' ps { psPos = psPos ps + 1 }
      _ <- match RParen ps1
      return (e, ps1 { psPos = psPos ps1 + 1 })
    LBracket -> do
      (es, ps1) <- parseList ps { psPos = psPos ps + 1 }
      _ <- match RBracket ps1
      return (Located (STList es) loc, ps1 { psPos = psPos ps1 + 1 })
    Underscore ->
      return (Located (STVar (T.pack "_")) loc, ps { psPos = psPos ps + 1 })
    _ -> fail' ("Unexpected token in term: " ++ show (tokenType tok)) ps

{-| Parse application arguments (greedy).
-}
parseAppArgs :: Parser [Located SurfaceTerm]
parseAppArgs ps =
  case peek ps of
    Right (tok, _)
      | canStartAtom (tokenType tok) -> do
          (arg, ps1) <- parseAtomTerm ps
          (rest, ps2) <- parseAppArgs ps1
          return (arg : rest, ps2)
    _ -> return ([], ps)

{-| Check if a token can start an atomic term.
-}
canStartAtom :: TokenType -> Bool
canStartAtom = \case
  Ident _ -> True
  QualIdent _ -> True
  IntLiteral _ -> True
  StringLiteral _ -> True
  KwTrue -> True
  KwFalse -> True
  LParen -> True
  LBracket -> True
  Backslash -> True
  KwLet -> True
  KwCase -> True
  KwForall -> True
  _ -> False

{-| Parse a comma-separated list (for tuples and function arguments).
-}
parseList :: Parser [Located SurfaceTerm]
parseList ps =
  case peek ps of
    Right (tok, _)
      | tokenType tok == RBracket -> return ([], ps)
    _ -> do
      (e, ps1) <- parseTerm' ps
      case peek ps1 of
        Right (tok, _)
          | tokenType tok == Comma -> do
              (rest, ps2) <- parseList ps1 { psPos = psPos ps1 + 1 }
              return (e : rest, ps2)
        _ -> return ([e], ps1)

{-| Parse a clause list for case expressions.
-}
parseClauseList :: Parser [SurfaceClause]
parseClauseList ps =
  case peek ps of
    Right (tok, _)
      | tokenType tok == Pipe || tokenType tok == RBrace -> return ([], ps)
    _ -> do
      (pat, ps1) <- parsePattern ps
      _ <- match OpArrow ps1
      (body, ps2) <- parseTerm' ps1 { psPos = psPos ps1 + 1 }
      let clause = SurfaceClause pat body
      (more, ps3) <- case peek ps2 of
        Right (tok, _)
          | tokenType tok == Semicolon -> parseClauseList ps2 { psPos = psPos ps2 + 1 }
        _ -> return ([], ps2)
      return (clause : more, ps3)

{-| Parse an identifier list.
-}
parseIdentList :: Parser [SurfaceIdent]
parseIdentList ps =
  case peek ps of
    Right (tok, _)
      | Ident x <- tokenType tok -> do
          (rest, ps1) <- parseIdentList ps { psPos = psPos ps + 1 }
          return (x : rest, ps1)
    _ -> return ([], ps)

-- ============================================================================
-- TYPE PARSING
-- ============================================================================

{-| Parse a type.
-}
parseType' :: Parser SurfaceType
parseType' = parseTypePrec 0

{-| Parse a type with precedence.
-}
parseTypePrec :: Int -> Parser SurfaceType
parseTypePrec _ ps = do
  (left, ps') <- parseAtomType ps
  parseTypeRest left ps'

{-| Parse type operators (->).
-}
parseTypeRest :: SurfaceType -> Parser SurfaceType
parseTypeRest left ps =
  case peek ps of
    Right (tok, _)
      | tokenType tok == OpArrow -> do
          (_, ps1) <- consume ps
          (right, ps2) <- parseType' ps1
          let result = STyFun left right
          parseTypeRest result ps2
    _ -> return' left ps

{-| Parse an atomic type.
-}
parseAtomType :: Parser SurfaceType
parseAtomType = parseForallType `alt` parseBasicType

{-| Parse a forall type.
-}
parseForallType :: Parser SurfaceType
parseForallType ps = do
  _ <- match KwForall ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'forall'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  ty_opt <- try (match OpColon ps2) ps2
  (mty, ps3) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps2 { psPos = psPos ps2 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match OpDot ps3
  (body, ps4) <- parseType' ps3 { psPos = psPos ps3 + 1 }
  return (STyForall name mty body, ps4)

{-| Parse a basic type.
-}
parseBasicType :: Parser SurfaceType
parseBasicType ps = do
  tok <- consume ps
  let loc = tokenLoc tok
  case tokenType tok of
    Ident x -> return (STyVar x, ps { psPos = psPos ps + 1 })
    QualIdent xs -> do
      let qn = map T.pack xs
      (args, ps') <- parseTypeArgs ps { psPos = psPos ps + 1 }
      return (STyConst qn args, ps')
    LParen -> do
      (ty, ps1) <- parseType' ps { psPos = psPos ps + 1 }
      _ <- match RParen ps1
      return (ty, ps1 { psPos = psPos ps1 + 1 })
    _ -> fail' ("Unexpected token in type: " ++ show (tokenType tok)) ps

{-| Parse type arguments.
-}
parseTypeArgs :: Parser [SurfaceType]
parseTypeArgs ps =
  case peek ps of
    Right (tok, _)
      | canStartAtomType (tokenType tok) -> do
          (arg, ps1) <- parseAtomType ps
          (rest, ps2) <- parseTypeArgs ps1
          return (arg : rest, ps2)
    _ -> return ([], ps)

{-| Check if token can start a type.
-}
canStartAtomType :: TokenType -> Bool
canStartAtomType = \case
  Ident _ -> True
  QualIdent _ -> True
  LParen -> True
  KwForall -> True
  _ -> False

-- ============================================================================
-- PATTERN PARSING
-- ============================================================================

{-| Parse a pattern.
-}
parsePattern :: Parser (Located SurfacePattern)
parsePattern ps = do
  tok <- consume ps
  let loc = tokenLoc tok
  case tokenType tok of
    Ident x -> return (Located (SPVar x) loc, ps { psPos = psPos ps + 1 })
    QualIdent xs -> do
      let qn = map T.pack xs
      (pats, ps') <- parsePatternArgs ps { psPos = psPos ps + 1 }
      return (Located (SPConstr qn pats) loc, ps')
    Underscore -> return (Located SPWildcard loc, ps { psPos = psPos ps + 1 })
    IntLiteral n -> return (Located (SPLit (IntLit n)) loc, ps { psPos = psPos ps + 1 })
    KwTrue -> return (Located (SPLit (BoolLit True)) loc, ps { psPos = psPos ps + 1 })
    KwFalse -> return (Located (SPLit (BoolLit False)) loc, ps { psPos = psPos ps + 1 })
    LParen -> do
      (p, ps1) <- parsePattern ps { psPos = psPos ps + 1 }
      _ <- match RParen ps1
      return (p, ps1 { psPos = psPos ps1 + 1 })
    _ -> fail' ("Unexpected token in pattern: " ++ show (tokenType tok)) ps

{-| Parse pattern arguments.
-}
parsePatternArgs :: Parser [SurfacePattern]
parsePatternArgs ps =
  case peek ps of
    Right (tok, _)
      | canStartPattern (tokenType tok) -> do
          (Located pat _, ps1) <- parsePattern ps
          (rest, ps2) <- parsePatternArgs ps1
          return (pat : rest, ps2)
    _ -> return ([], ps)

{-| Check if token can start a pattern.
-}
canStartPattern :: TokenType -> Bool
canStartPattern = \case
  Ident _ -> True
  QualIdent _ -> True
  Underscore -> True
  IntLiteral _ -> True
  KwTrue -> True
  KwFalse -> True
  LParen -> True
  _ -> False

-- ============================================================================
-- PROPOSITION PARSING
-- ============================================================================

{-| Parse a proposition.
-}
parseProposition' :: Parser (Located SurfaceProposition)
parseProposition' = parsePropositionPrec 0

{-| Parse a proposition with precedence.
-}
parsePropositionPrec :: Int -> Parser (Located SurfaceProposition)
parsePropositionPrec minPrec ps = do
  (left, ps') <- parseAtomProposition ps
  parsePropositionRest minPrec left ps'

{-| Parse proposition operators.
-}
parsePropositionRest :: Int -> Located SurfaceProposition -> Parser (Located SurfaceProposition)
parsePropositionRest minPrec left ps =
  case peek ps of
    Right (tok, _)
      | isLogicalOp (tokenType tok) ->
          let prec = logicalPrecedence (tokenType tok)
          in if prec >= minPrec
             then do
               (op, ps1) <- consume ps
               (right, ps2) <- parsePropositionPrec (prec + 1) ps1
               let result = case tokenType op of
                     OpAnd -> Located (SPAnd left right) (locLoc left)
                     OpOr -> Located (SPOr left right) (locLoc left)
                     _ -> left
               parsePropositionRest minPrec result ps2
             else return' left ps
    _ -> return' left ps

{-| Check if a token is a logical operator.
-}
isLogicalOp :: TokenType -> Bool
isLogicalOp = \case
  OpAnd -> True
  OpOr -> True
  _ -> False

{-| Get logical operator precedence.
-}
logicalPrecedence :: TokenType -> Int
logicalPrecedence = \case
  OpAnd -> 2
  OpOr -> 1
  _ -> 0

{-| Parse a raw proposition (without location).
-}
parsePropositionRaw :: Parser SurfaceProposition
parsePropositionRaw ps = do
  (Located p _, ps') <- parseProposition' ps
  return (p, ps')

{-| Parse an atomic proposition.
-}
parseAtomProposition :: Parser (Located SurfaceProposition)
parseAtomProposition = parseForallProposition `alt` parseExistsProposition
                      `alt` parseBasicProposition

{-| Parse a forall proposition.
-}
parseForallProposition :: Parser (Located SurfaceProposition)
parseForallProposition ps = do
  _ <- match KwForall ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'forall'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  ty_opt <- try (match OpColon ps2) ps2
  (mty, ps3) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps2 { psPos = psPos ps2 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match OpDot ps3
  (body, ps4) <- parseProposition' ps3 { psPos = psPos ps3 + 1 }
  let loc = tokenLoc nameToken
  return (Located (SPForall name mty body) loc, ps4)

{-| Parse an exists proposition.
-}
parseExistsProposition :: Parser (Located SurfaceProposition)
parseExistsProposition ps = do
  _ <- match (Ident (T.pack "exists")) ps
  (nameToken, ps1) <- peek ps
  name <- case tokenType nameToken of
    Ident x -> return x
    _ -> fail' "Expected identifier after 'exists'" ps
  ps2 <- return $ ps1 { psPos = psPos ps1 + 1 }
  ty_opt <- try (match OpColon ps2) ps2
  (mty, ps3) <- case ty_opt of
    Just _ -> do
      (ty, ps') <- parseType' ps2 { psPos = psPos ps2 + 1 }
      return (Just ty, ps')
    Nothing -> return (Nothing, ps2)
  _ <- match OpDot ps3
  (body, ps4) <- parseProposition' ps3 { psPos = psPos ps3 + 1 }
  let loc = tokenLoc nameToken
  return (Located (SPExists name mty body) loc, ps4)

{-| Parse a basic proposition (equality or atom).
-}
parseBasicProposition :: Parser (Located SurfaceProposition)
parseBasicProposition ps = do
  (e1, ps1) <- parseTerm' ps
  case peek ps1 of
    Right (tok, _)
      | tokenType tok == OpEq -> do
          (_, ps2) <- consume ps1
          (e2, ps3) <- parseTerm' ps2
          return (Located (SPEq e1 e2) (locLoc e1), ps3)
    _ -> fail' "Expected '==' in proposition" ps

-- ============================================================================
-- PROOF PARSING
-- ============================================================================

{-| Parse a proof.
-}
parseProof' :: Parser (Located SurfaceProof)
parseProof' ps = do
  tok <- consume ps
  let loc = tokenLoc tok
  case tokenType tok of
    KwRefl -> return (Located SPRefl loc, ps { psPos = psPos ps + 1 })
    KwSymm -> do
      (p, ps1) <- parseProof' ps { psPos = psPos ps + 1 }
      return (Located (SPSymm p) loc, ps1)
    KwTrans -> do
      (p1, ps1) <- parseProof' ps { psPos = psPos ps + 1 }
      (p2, ps2) <- parseProof' ps1
      return (Located (SPTrans p1 p2) loc, ps2)
    Backslash -> do
      (vars, ps1) <- parseIdentList ps { psPos = psPos ps + 1 }
      _ <- match OpArrow ps1
      (body, ps2) <- parseProof' ps1 { psPos = psPos ps1 + 1 }
      return (Located (SPIntro vars body) loc, ps2)
    Ident x -> do
      case T.unpack x of
        "by_refl" -> return (Located (SPByName (T.pack "by_refl")) loc, ps { psPos = psPos ps + 1 })
        "by_simp" -> return (Located (SPByName (T.pack "by_simp")) loc, ps { psPos = psPos ps + 1 })
        _ -> return (Located (SPVar x) loc, ps { psPos = psPos ps + 1 })
    QualIdent xs -> return (Located (SPOpaque (map T.pack xs)) loc, ps { psPos = psPos ps + 1 })
    _ -> fail' ("Unexpected token in proof: " ++ show (tokenType tok)) ps

-- ============================================================================
-- ASSERTION PARSING
-- ============================================================================

{-| Parse an assertion.
-}
parseAssertion' :: Parser SurfaceAssertion
parseAssertion' = parseAssertionDecl
