{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Assertica.Surface.AST
Description : Surface syntax abstract syntax tree (intermediate representation)
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The surface AST represents the intermediate form after parsing Haskell-like syntax
but before elaboration into the core AST. It preserves source locations for error
reporting and supports syntactic sugar that the elaborator will expand.

DESIGN PRINCIPLES:
1. Explicit source location tracking on all nodes
2. Surface-level syntactic sugar (qualified names, infix operators)
3. Minimal implicit argument information (explicit where possible)
4. Clear separation from core AST
-}

module Assertica.Surface.AST
  ( -- * Source location
    SourceLoc (..)
  , Located (..)

    -- * Surface names
  , SurfaceIdent
  , SurfaceQName

    -- * Surface syntax terms
  , SurfaceTerm (..)
  , SurfaceType (..)
  , SurfacePattern (..)
  , SurfaceClause (..)

    -- * Propositions and assertions
  , SurfaceProposition (..)
  , SurfaceAssertion (..)
  , SurfaceProof (..)

    -- * Type and data declarations
  , SurfaceDecl (..)
  , SurfaceDataCon (..)

    -- * Constants
  , SurfaceConstant (..)

    -- * Pretty printing
  , prettySurfaceTerm
  , prettySurfaceType
  , prettySurfaceProposition
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.List (intercalate)

-- ============================================================================
-- SOURCE LOCATION TRACKING
-- ============================================================================

{-| Source location for error reporting.
-}
data SourceLoc = SourceLoc
  { slFile   :: String   -- ^ File path
  , slLine   :: Int      -- ^ Line number (1-indexed)
  , slCol    :: Int      -- ^ Column number (1-indexed)
  }
  deriving (Eq, Show, Generic)

{-| A value with source location information.
-}
data Located a = Located
  { locValue :: a
  , locLoc   :: SourceLoc
  }
  deriving (Eq, Show, Functor, Traversable, Foldable, Generic)

-- ============================================================================
-- SURFACE NAMES
-- ============================================================================

{-| A simple identifier (e.g., "x", "join", "True")
-}
type SurfaceIdent = Text

{-| A qualified name (e.g., "Prelude.Bool" or just "Bool")
Parsed as segments separated by dots.
-}
type SurfaceQName = [Text]

-- ============================================================================
-- SURFACE CONSTANTS
-- ============================================================================

{-| Surface-level constants.
-}
data SurfaceConstant
  = IntLit Integer         -- ^ Integer literal: 42, -5
  | BoolLit Bool           -- ^ Boolean literal: True, False
  | StringLit String       -- ^ String literal: "hello"
  | UnitLit                -- ^ Unit: ()
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE TYPES
-- ============================================================================

{-| Surface-level types.
Supports the same level of expressiveness as core types but with syntax sugar.
-}
data SurfaceType
  = STyVar SurfaceIdent                      -- ^ Type variable: a, b
  | STyConst SurfaceQName [SurfaceType]      -- ^ Named type: Int, List a
  | STyFun SurfaceType SurfaceType           -- ^ Function type: a -> b
  | STyForall SurfaceIdent (Maybe SurfaceType) SurfaceType
                                             -- ^ Forall: forall x. T or forall x : A. B
  | STyTuple [SurfaceType]                   -- ^ Tuple type: (a, b, c)
  | STyApp SurfaceType SurfaceType           -- ^ Type application
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE PATTERNS
-- ============================================================================

{-| Surface-level patterns for pattern matching.
-}
data SurfacePattern
  = SPVar SurfaceIdent                       -- ^ Variable pattern: x
  | SPConstr SurfaceQName [SurfacePattern]   -- ^ Constructor pattern: Cons x xs
  | SPWildcard                               -- ^ Wildcard pattern: _
  | SPLit SurfaceConstant                    -- ^ Literal pattern: 42, True
  | SPTuple [SurfacePattern]                 -- ^ Tuple pattern: (x, y, z)
  deriving (Eq, Show, Generic)

{-| A clause in a case expression: pattern -> body
-}
data SurfaceClause = SurfaceClause
  { scPattern :: Located SurfacePattern
  , scBody    :: Located SurfaceTerm
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE TERMS
-- ============================================================================

{-| Surface-level terms.
Includes syntactic sugar like infix operators, lambda with type annotations, etc.
-}
data SurfaceTerm
  = STVar SurfaceIdent                       -- ^ Variable: x, join
  | STConst SurfaceConstant                  -- ^ Constant: 42, True, "hello"
  | STLam [SurfaceIdent] (Maybe SurfaceType) (Located SurfaceTerm)
                                             -- ^ Lambda: \x -> e or \x : A -> e or \(x y) : A -> e
  | STApp (Located SurfaceTerm) [(Located SurfaceTerm)]
                                             -- ^ Application: f x y z (func + list of args)
  | STInfix (Located SurfaceTerm) SurfaceIdent (Located SurfaceTerm)
                                             -- ^ Infix application: a + b (converted to "+" a b)
  | STLet SurfaceIdent (Maybe SurfaceType) (Located SurfaceTerm) (Located SurfaceTerm)
                                             -- ^ Let binding: let x = e1 in e2
  | STCase (Located SurfaceTerm) [SurfaceClause] (Maybe (Located SurfaceTerm))
                                             -- ^ Case expression with optional default
  | STAnn (Located SurfaceTerm) SurfaceType  -- ^ Type annotation: e : T
  | STForall SurfaceIdent (Maybe SurfaceType) (Located SurfaceTerm)
                                             -- ^ Dependent function: forall x : A . body
  | STTuple [Located SurfaceTerm]            -- ^ Tuple: (e1, e2, e3)
  | STList [Located SurfaceTerm]             -- ^ List: [e1, e2, e3]
  | STConstr SurfaceQName [Located SurfaceTerm]
                                             -- ^ Data constructor: Just 5, Cons x xs
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE PROPOSITIONS
-- ============================================================================

{-| Surface-level propositions (logical claims).
-}
data SurfaceProposition
  = SPEq (Located SurfaceTerm) (Located SurfaceTerm)
                                             -- ^ Equality: a == b
  | SPForall SurfaceIdent (Maybe SurfaceType) (Located SurfaceProposition)
                                             -- ^ Forall: forall x : A. P x
  | SPExists SurfaceIdent (Maybe SurfaceType) (Located SurfaceProposition)
                                             -- ^ Exists: exists x : A. P x
  | SPAnd (Located SurfaceProposition) (Located SurfaceProposition)
                                             -- ^ Conjunction: P /\ Q
  | SPOr (Located SurfaceProposition) (Located SurfaceProposition)
                                             -- ^ Disjunction: P \/ Q
  | SPImpl (Located SurfaceProposition) (Located SurfaceProposition)
                                             -- ^ Implication: P -> Q
  | SPNot (Located SurfaceProposition)       -- ^ Negation: not P
  | SPTrue                                   -- ^ True: True
  | SPFalse                                  -- ^ False: False
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE PROOFS
-- ============================================================================

{-| Surface-level proofs.
-}
data SurfaceProof
  = SPRefl                                   -- ^ Reflexivity: refl or by_refl
  | SPSymm (Located SurfaceProof)            -- ^ Symmetry: symm P
  | SPTrans (Located SurfaceProof) (Located SurfaceProof)
                                             -- ^ Transitivity: trans P1 P2
  | SPIntro [SurfaceIdent] (Located SurfaceProof)
                                             -- ^ Intro: fun x => P
  | SPVar SurfaceIdent                       -- ^ Proof variable
  | SPOpaque SurfaceQName                    -- ^ Opaque proof (axiom/oracle)
  | SPByName SurfaceIdent                    -- ^ Named proof strategy: by_refl, by_simp
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE ASSERTIONS
-- ============================================================================

{-| Surface-level assertion declaration.
-}
data SurfaceAssertion = SurfaceAssertion
  { saName :: Located SurfaceIdent
  , saProp :: Located SurfaceProposition
  , saProof :: Maybe (Located SurfaceProof)
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- SURFACE DECLARATIONS
-- ============================================================================

{-| Surface-level data constructor in a data type declaration.
-}
data SurfaceDataCon = SurfaceDataCon
  { sdcName :: SurfaceIdent
  , sdcArgs :: [SurfaceType]
  }
  deriving (Eq, Show, Generic)

{-| Surface-level declarations (type, data, function, assertion, proof, typeclass).
-}
data SurfaceDecl
  = SDeclType SurfaceIdent SurfaceType       -- ^ Type alias: type T = A
  | SDeclData SurfaceIdent [SurfaceIdent] [SurfaceDataCon]
                                             -- ^ Data: data T a = C1 | C2 ...
  | SDeclFunc SurfaceIdent (Maybe SurfaceType) [(Located SurfaceTerm)]
                                             -- ^ Function: f : A -> B; f x = body
  | SDeclAssertion SurfaceAssertion          -- ^ Assertion: assert name : prop
  | SDeclProof SurfaceIdent SurfaceProposition (Located SurfaceProof)
                                             -- ^ Proof: proof name : prop := proof_term
  deriving (Eq, Show, Generic)

-- ============================================================================
-- PRETTY PRINTING
-- ============================================================================

prettySurfaceTerm :: Located SurfaceTerm -> String
prettySurfaceTerm (Located term _) = prettySurfaceTermRaw term

prettySurfaceTermRaw :: SurfaceTerm -> String
prettySurfaceTermRaw = \case
  STVar x -> T.unpack x
  STConst c -> prettySurfaceConstant c
  STLam xs mty body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "\\" ++ unwords (map T.unpack xs) ++ ty_str ++ " -> " ++ prettySurfaceTerm body
  STApp func args ->
    "(" ++ prettySurfaceTerm func ++ " " ++ unwords (map prettySurfaceTerm args) ++ ")"
  STInfix l op r ->
    "(" ++ prettySurfaceTerm l ++ " " ++ T.unpack op ++ " " ++ prettySurfaceTerm r ++ ")"
  STLet x mty e1 e2 ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "let " ++ T.unpack x ++ ty_str ++ " = " ++ prettySurfaceTerm e1 ++ " in " ++ prettySurfaceTerm e2
  STCase e clauses def ->
    "case " ++ prettySurfaceTerm e ++ " of { " ++
    intercalate "; " [prettySurfacePattern (locValue (scPattern c)) ++ " -> " ++ prettySurfaceTerm (scBody c) | c <- clauses] ++
    (case def of Nothing -> ""; Just d -> "; _ -> " ++ prettySurfaceTerm d) ++ " }"
  STAnn e ty -> "(" ++ prettySurfaceTerm e ++ " : " ++ prettySurfaceType ty ++ ")"
  STForall x mty body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "forall " ++ T.unpack x ++ ty_str ++ " . " ++ prettySurfaceTerm body
  STTuple es -> "(" ++ intercalate ", " (map prettySurfaceTerm es) ++ ")"
  STList es -> "[" ++ intercalate ", " (map prettySurfaceTerm es) ++ "]"
  STConstr qn args ->
    let qn_str = intercalate "." (map T.unpack qn)
    in qn_str ++ if null args then "" else " " ++ unwords (map prettySurfaceTerm args)

prettySurfacePattern :: SurfacePattern -> String
prettySurfacePattern = \case
  SPVar x -> T.unpack x
  SPConstr qn pats ->
    let qn_str = intercalate "." (map T.unpack qn)
    in qn_str ++ if null pats then "" else " " ++ unwords (map prettySurfacePattern pats)
  SPWildcard -> "_"
  SPLit c -> prettySurfaceConstant c
  SPTuple ps -> "(" ++ intercalate ", " (map prettySurfacePattern ps) ++ ")"

prettySurfaceType :: SurfaceType -> String
prettySurfaceType = \case
  STyVar x -> T.unpack x
  STyConst qn tys ->
    let qn_str = intercalate "." (map T.unpack qn)
    in if null tys
       then qn_str
       else qn_str ++ " " ++ unwords (map prettySurfaceType tys)
  STyFun a b -> "(" ++ prettySurfaceType a ++ " -> " ++ prettySurfaceType b ++ ")"
  STyForall x mty body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "forall " ++ T.unpack x ++ ty_str ++ " . " ++ prettySurfaceType body
  STyTuple tys -> "(" ++ intercalate ", " (map prettySurfaceType tys) ++ ")"
  STyApp a b -> "(" ++ prettySurfaceType a ++ " " ++ prettySurfaceType b ++ ")"

prettySurfaceConstant :: SurfaceConstant -> String
prettySurfaceConstant = \case
  IntLit n -> show n
  BoolLit b -> show b
  StringLit s -> show s
  UnitLit -> "()"

prettySurfaceProposition :: Located SurfaceProposition -> String
prettySurfaceProposition (Located prop _) = prettySurfacePropositionRaw prop

prettySurfacePropositionRaw :: SurfaceProposition -> String
prettySurfacePropositionRaw = \case
  SPEq l r -> "(" ++ prettySurfaceTerm l ++ " == " ++ prettySurfaceTerm r ++ ")"
  SPForall x mty body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "forall " ++ T.unpack x ++ ty_str ++ " . " ++ prettySurfaceProposition body
  SPExists x mty body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettySurfaceType ty
    in "exists " ++ T.unpack x ++ ty_str ++ " . " ++ prettySurfaceProposition body
  SPAnd p1 p2 -> "(" ++ prettySurfaceProposition p1 ++ " /\\ " ++ prettySurfaceProposition p2 ++ ")"
  SPOr p1 p2 -> "(" ++ prettySurfaceProposition p1 ++ " \\/ " ++ prettySurfaceProposition p2 ++ ")"
  SPImpl p1 p2 -> "(" ++ prettySurfaceProposition p1 ++ " -> " ++ prettySurfaceProposition p2 ++ ")"
  SPNot p -> "not (" ++ prettySurfaceProposition p ++ ")"
  SPTrue -> "True"
  SPFalse -> "False"
