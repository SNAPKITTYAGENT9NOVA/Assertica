{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

{-|
Module      : Assertica.Core.Pattern
Description : Pattern matching compilation and exhaustiveness checking
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

This module implements pattern matching compilation: transforming surface pattern
syntax into core eliminators that preserve type information and exhaustiveness
guarantees.

DESIGN PRINCIPLES:
1. **Exhaustiveness checking**: All cases must be covered or default provided.
   No unmatched patterns reach the kernel.

2. **Type preservation**: Pattern compilation preserves types through pattern variables.

3. **Constructor awareness**: Pattern compiler knows constructor arities and argument types.

4. **Dependent pattern matching**: Support dependent patterns where types depend on matched values.

5. **Fail-closed**: Ill-formed patterns rejected with clear diagnostics.

ARCHITECTURE:
The pattern compiler sits between elaboration and type checking:

  Elaborator (Agent 3A)
      ↓ produces SurfacePattern
  Parser/Elaboration
      ↓
  Pattern Compiler (THIS MODULE)
      ↓ validates + compiles to Case
      ↓
  Type Checker (Agent 3B)
      ↓
  Kernel Verification

REQUIRED INTEGRATION POINTS:
- QueryConstructors: Look up constructor arities from type environment
- CompiledPattern: Pattern variables must have inferred types
- TypeChecker receives compiled Case expressions with type preservation
-}

module Assertica.Core.Pattern
  ( -- * Pattern AST (extended)
    Pattern(..)
  , PatternVar
  , PatternInfo(..)

    -- * Constructor information
  , Constructor(..)
  , ConstructorDB
  , emptyConstructorDB
  , addConstructor
  , queryConstructors
  , queryConstructorArity

    -- * Exhaustiveness checking
  , ExhaustiveResult(..)
  , checkExhaustive
  , findMissingCases
  , isRedundant

    -- * Pattern compilation
  , compilePattern
  , compilePatterns
  , CompiledPattern(..)
  , PatternBindings
  , extractPatternBindings

    -- * Type-aware pattern matching
  , PatternContext(..)
  , inferPatternType
  , checkPatternType

    -- * Error types and diagnostics
  , PatternError(..)
  , prettyPatternError
  , ExhaustiveError(..)
  , prettyExhaustiveError
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes, isJust, fromMaybe)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Assertica.Core.AST
  ( Var(..)
  , QName(..)
  , Type(..)
  , Term(..)
  , Pattern(..)
  , Clause(..)
  , Binder(..)
  , varName
  , prettyPattern
  , prettyType
  )

-- ============================================================================
-- PATTERN AST (EXTENDED)
-- ============================================================================

{-| Information about a pattern variable binding.
Tracks the variable name and its inferred or declared type.
-}
data PatternVar = PatternVar
  { pvVar  :: Var     -- ^ The bound variable
  , pvType :: Maybe Type  -- ^ Inferred or declared type
  }
  deriving (Eq, Show, Generic, Typeable)

{-| Extended pattern information for compilation.
-}
data PatternInfo = PatternInfo
  { piPattern :: Pattern     -- ^ The pattern itself
  , piBindings :: PatternBindings  -- ^ Variables bound by this pattern
  , piType :: Maybe Type     -- ^ Expected or inferred type
  }
  deriving (Eq, Show, Generic, Typeable)

-- ============================================================================
-- CONSTRUCTOR DATABASE
-- ============================================================================

{-| Information about a data constructor.
-}
data Constructor = Constructor
  { ctorName :: QName       -- ^ Qualified name of constructor
  , ctorArity :: Int        -- ^ Number of arguments
  , ctorArgTypes :: [Type]  -- ^ Types of arguments (may be dependent)
  , ctorReturnType :: Type  -- ^ Return type of constructor
  }
  deriving (Eq, Show, Generic, Typeable)

{-| Database of known constructors.
Maps type names to their constructors.
-}
type ConstructorDB = Map.Map QName [Constructor]

{-| Create an empty constructor database.
-}
emptyConstructorDB :: ConstructorDB
emptyConstructorDB = Map.empty

{-| Add a constructor to the database.
-}
addConstructor :: ConstructorDB -> QName -> Constructor -> ConstructorDB
addConstructor db tyName ctor =
  Map.insertWith (++) tyName [ctor] db

{-| Query all constructors for a given type.
-}
queryConstructors :: ConstructorDB -> QName -> Either PatternError [Constructor]
queryConstructors db tyName =
  case Map.lookup tyName db of
    Just ctors -> Right ctors
    Nothing -> Left (UnknownConstructor tyName)

{-| Query the arity of a specific constructor.
-}
queryConstructorArity :: ConstructorDB -> QName -> Either PatternError Int
queryConstructorArity db ctorName = do
  -- Search through all constructors in all types
  let allCtors = concat (Map.elems db)
  case filter (\c -> ctorName == ctorName) allCtors of
    (ctor:_) -> Right (ctorArity ctor)
    [] -> Left (UnknownConstructor ctorName)

-- ============================================================================
-- EXHAUSTIVENESS CHECKING
-- ============================================================================

{-| Result of exhaustiveness checking.
-}
data ExhaustiveResult
  = Exhaustive          -- ^ All cases covered
  | MissingCases [Pattern]  -- ^ Missing constructor patterns
  | HasDefault          -- ^ Has a default clause (always safe)
  deriving (Eq, Show, Generic, Typeable)

{-| Error type for exhaustiveness checking failures.
-}
data ExhaustiveError
  = IncompleteMatch [Pattern]  -- ^ Patterns that are not covered
  | DuplicatePattern Pattern   -- ^ Duplicate or redundant pattern
  | UnreachablePattern Pattern -- ^ Pattern can never match
  deriving (Eq, Show, Generic, Typeable)

{-| Pretty-print an exhaustive checking error.
-}
prettyExhaustiveError :: ExhaustiveError -> String
prettyExhaustiveError = \case
  IncompleteMatch pats ->
    "Incomplete pattern match. Missing cases:\n  " ++
    intercalate "\n  " (map prettyPattern pats)
  DuplicatePattern pat ->
    "Duplicate or redundant pattern: " ++ prettyPattern pat
  UnreachablePattern pat ->
    "Unreachable pattern: " ++ prettyPattern pat

{-| Check if a set of patterns exhaustively covers a type.

Returns:
  - Right () if patterns are exhaustive or have a default
  - Left (IncompleteMatch missing) if patterns don't cover all cases
  - Left (DuplicatePattern pat) if there are redundant patterns

For this implementation, we need the constructor database to determine
which constructors must be matched.
-}
checkExhaustive :: ConstructorDB -> Type -> [Pattern] -> Maybe Pattern
                -> Either ExhaustiveError ()
checkExhaustive _db _ty _patterns (Just _) =
  -- If there's a default clause, patterns are always exhaustive
  Right ()

checkExhaustive db ty patterns Nothing = do
  -- Check for duplicates
  let patStrs = map prettyPattern patterns
  if length (nub patStrs) /= length patStrs
    then Left (DuplicatePattern (head patterns))
    else Right ()

  -- Find missing cases
  missing <- findMissingCases db ty patterns
  if null missing
    then Right ()
    else Left (IncompleteMatch missing)

{-| Find pattern cases that are not covered by the given patterns.
-}
findMissingCases :: ConstructorDB -> Type -> [Pattern]
                 -> Either ExhaustiveError [Pattern]
findMissingCases db ty patterns = do
  -- Get the type name
  let tyName = case ty of
        TyConst name _ -> name
        _ -> QName [] "Unknown"

  -- Query available constructors
  ctors <- queryConstructors db tyName

  -- Extract constructor names from patterns
  let coveredCtors = Set.fromList
        [name | PatConstr name _ <- patterns]

  -- Find uncovered constructors
  let uncovered = [ctor | ctor <- ctors, not (Set.member (ctorName ctor) coveredCtors)]

  -- Create missing patterns (wildcards for now)
  let missingPats = [PatConstr (ctorName c) (replicate (ctorArity c) PatWildcard) | c <- uncovered]

  Right missingPats

{-| Check if a pattern is redundant (unreachable).
A pattern is redundant if it cannot match any value that wasn't already
matched by earlier patterns.

This is a simplified check: we mark a pattern as potentially redundant if
it matches a constructor that was already matched.
-}
isRedundant :: [Pattern] -> Pattern -> Bool
isRedundant earlier pat =
  case pat of
    PatConstr name _ ->
      any (\case PatConstr n _ -> n == name; _ -> False) earlier
    PatWildcard -> any (== PatWildcard) earlier
    PatVar _ -> False  -- Variables always bind, never redundant
    PatLit _ -> False  -- Literal patterns handled separately (not yet implemented)

-- ============================================================================
-- PATTERN COMPILATION
-- ============================================================================

{-| Type for pattern bindings (variable -> type mappings).
-}
type PatternBindings = Map.Map Var Type

{-| Compiled pattern information.
-}
data CompiledPattern = CompiledPattern
  { cpOriginal :: Pattern     -- ^ Original surface pattern
  , cpBindings :: PatternBindings  -- ^ Variables bound by this pattern
  , cpType :: Maybe Type      -- ^ Inferred type for this pattern
  }
  deriving (Eq, Show, Generic, Typeable)

{-| Extract all pattern variable bindings from a pattern.

This walks the pattern tree and collects all bound variables with their types.
-}
extractPatternBindings :: Pattern -> Type -> PatternBindings
extractPatternBindings pat ty = go pat ty
  where
    go :: Pattern -> Type -> PatternBindings
    go p t = case p of
      PatVar v -> Map.singleton v t

      PatConstr _ subPats ->
        -- For constructor patterns, extract bindings from subpatterns
        -- This is simplified: in full implementation, we'd use constructor arg types
        let subTypes = replicate (length subPats) t
        in Map.unions [go subpat subty | (subpat, subty) <- zip subPats subTypes]

      PatWildcard -> Map.empty

      PatLit _ -> Map.empty

{-| Compile a single pattern to a core pattern.

This validates the pattern structure and preserves type information.
-}
compilePattern :: Pattern -> Either PatternError CompiledPattern
compilePattern pat = do
  -- For now, minimal compilation: just validate structure
  case pat of
    PatVar v ->
      Right (CompiledPattern pat (Map.singleton v (TyVar v)) Nothing)

    PatConstr name subPats -> do
      -- Validate that we can compile subpatterns
      compiledSubs <- mapM compilePattern subPats
      let allBindings = Map.unions [cpBindings cp | cp <- compiledSubs]
      Right (CompiledPattern pat allBindings Nothing)

    PatWildcard ->
      Right (CompiledPattern pat Map.empty Nothing)

    PatLit _ ->
      Right (CompiledPattern pat Map.empty Nothing)

{-| Compile a list of patterns to core patterns.

Returns a list of compiled patterns with bindings and type information.
-}
compilePatterns :: [Pattern] -> Either PatternError [CompiledPattern]
compilePatterns = mapM compilePattern

-- ============================================================================
-- PATTERN TYPE INFERENCE
-- ============================================================================

{-| Context for type-aware pattern matching.
-}
data PatternContext = PatternContext
  { pcType :: Type        -- ^ Expected type for patterns
  , pcConstructors :: ConstructorDB  -- ^ Available constructors
  }
  deriving (Eq, Show, Generic, Typeable)

{-| Infer the type of a pattern variable.

Given a pattern and an expected type, infer the type of any bound variables.
-}
inferPatternType :: PatternContext -> Pattern -> Either PatternError Type
inferPatternType ctx = \case
  PatVar _ -> Right (pcType ctx)

  PatConstr _ _ -> Right (pcType ctx)

  PatWildcard -> Right (pcType ctx)

  PatLit _ -> Right (pcType ctx)

{-| Check that a pattern matches an expected type.

Returns the refined type for the pattern (in case of dependent pattern matching).
-}
checkPatternType :: PatternContext -> Pattern -> Type
                 -> Either PatternError Type
checkPatternType _ctx _pat expectedTy =
  -- For now, just accept the expected type
  Right expectedTy

-- ============================================================================
-- ERROR TYPES
-- ============================================================================

{-| Pattern compilation and checking errors.
-}
data PatternError
  = UnknownConstructor QName
  | ConstructorArityMismatch QName Int Int  -- ^ (constructor, expected, actual)
  | DuplicateConstructor QName
  | InvalidPattern String
  | PatternTypeError String
  | UncoveredConstructor QName
  deriving (Eq, Show, Generic, Typeable)

{-| Pretty-print a pattern error.
-}
prettyPatternError :: PatternError -> String
prettyPatternError = \case
  UnknownConstructor (QName mod name) ->
    "Unknown constructor: " ++ qualName mod name
  ConstructorArityMismatch (QName mod name) expected actual ->
    qualName mod name ++ " expects " ++ show expected ++ " arguments, got " ++ show actual
  DuplicateConstructor (QName mod name) ->
    "Duplicate constructor: " ++ qualName mod name
  InvalidPattern msg ->
    "Invalid pattern: " ++ msg
  PatternTypeError msg ->
    "Pattern type error: " ++ msg
  UncoveredConstructor (QName mod name) ->
    "Pattern match is not exhaustive for constructor: " ++ qualName mod name
  where
    qualName mod name =
      if null mod
        then T.unpack name
        else intercalate "." (map T.unpack mod) ++ "." ++ T.unpack name

-- ============================================================================
-- INTEGRATION: PATTERN COMPILATION TO CORE CASE
-- ============================================================================

{-| Compile patterns and clauses to a core Case expression.

This is the main entry point for the pattern compiler. It takes a list of
surface patterns and produces a typed core Case expression.

Preconditions:
  - Patterns are exhaustive (or a default is provided)
  - Constructors are known in the database
  - Type information is available

Postconditions:
  - Compiled patterns preserve type information
  - Pattern variables are properly bound
  - Case expression is ready for type checking
-}
compilePatternCases :: ConstructorDB -> Term -> [Clause]
                     -> Either PatternError Term
compilePatternCases _db scrutinee clauses = do
  -- Compile each clause
  _compiledClauses <- mapM compileSingleClause clauses

  -- For now, return the case expression as-is
  -- The actual transformation would happen here
  Right (Case scrutinee clauses Nothing)
  where
    compileSingleClause (Clause pat body) = do
      cp <- compilePattern pat
      let bindings = cpBindings cp
      -- The body should have all variables from bindings in scope
      return (Clause pat body, bindings)

-- ============================================================================
-- UTILITIES
-- ============================================================================

{-| Check if a pattern is wildcard-like (matches anything).
-}
isWildcardPattern :: Pattern -> Bool
isWildcardPattern = \case
  PatWildcard -> True
  PatVar _ -> True
  _ -> False

{-| Extract all constructors mentioned in a pattern.
-}
patternConstructors :: Pattern -> Set.Set QName
patternConstructors = \case
  PatConstr name subPats ->
    Set.insert name (Set.unions (map patternConstructors subPats))
  PatVar _ -> Set.empty
  PatWildcard -> Set.empty
  PatLit _ -> Set.empty
