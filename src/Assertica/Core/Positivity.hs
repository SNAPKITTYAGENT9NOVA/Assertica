{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveShow #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Core.Positivity
Description : Positivity checking for inductive type definitions
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

This module implements positivity checking to ensure inductive type definitions
are strictly positive. A type parameter is strictly positive if it appears only
in positive positions (as data constructor arguments), never in negative positions
(such as function domain types).

DEFINITION: Strict Positivity
- A type variable 'a' is POSITIVE in a type if it appears only in positions where
  it could not lead to inconsistency (e.g., as constructor argument)
- A type variable 'a' is NEGATIVE if it appears in a function domain (a -> T)
- A type is STRICTLY POSITIVE if all type parameters appear only in positive positions

EXAMPLES:
- `data List a = Nil | Cons a (List a)` is strictly positive
  - 'a' appears as constructor argument (positive position)
  - 'List a' appears as constructor argument (positive position)

- `data Tree a = Leaf a | Node (Tree a) (Tree a)` is strictly positive
  - 'a' appears as constructor argument
  - Recursive occurrences are in positive positions

- `data BadType a = Bad (a -> Int)` is NOT strictly positive
  - 'a' appears in function domain (negative position)

- `data BadList a = Cons a (BadList (a -> Int))` is NOT strictly positive
  - 'a' appears in negative position via function type argument

DESIGN PRINCIPLES:
1. **Fail-closed**: Position violations are always rejected
2. **Occurs-check**: Track all occurrences of type parameters
3. **Recursive tracking**: Handle nested type structures
4. **Clear diagnostics**: Report exactly which position violates positivity
5. **Integration**: Works with type definitions from the AST

ALGORITHM:
1. For each type definition, extract type parameters
2. For each constructor, analyze argument types
3. Track position context (positive/negative)
4. When entering function domain, flip position
5. When entering constructor argument, maintain/flip based on nesting
6. Report violations with location information
-}

module Assertica.Core.Positivity
  ( -- * Main positivity checking
    checkPositivity
  , checkPositivityMulti
  , needsPositivityCheck

    -- * Error type
  , PosError(..)
  , prettyPosError

    -- * Analysis utilities (for testing)
  , analyzeTypeOccurrences
  , TypeOccurrence(..)
  , Position(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate)
import GHC.Generics (Generic)

import Assertica.Core.AST
import Assertica.Core.TypeEnv

-- ============================================================================
-- ERROR TYPES
-- ============================================================================

{-| Positivity checking errors. -}
data PosError
  = NonPositiveOccurrence Text String
    -- ^ Type parameter appears in negative position with explanation
  | TypeVarInFunctionDomain Text
    -- ^ Type variable appears in function argument type
  | NestedNegativeOccurrence Text String
    -- ^ Type parameter nested in negative position
  | ImpredicativeNesting String
    -- ^ Impredicative nesting detected
  deriving (Eq, Show, Generic)

{-| Pretty-print a positivity error for user display. -}
prettyPosError :: PosError -> String
prettyPosError = \case
  NonPositiveOccurrence param msg ->
    "Type parameter '" ++ T.unpack param ++
    "' appears in non-positive position: " ++ msg
  TypeVarInFunctionDomain param ->
    "Type parameter '" ++ T.unpack param ++
    "' appears in function domain (negative position)"
  NestedNegativeOccurrence param msg ->
    "Type parameter '" ++ T.unpack param ++
    "' nested in negative position: " ++ msg
  ImpredicativeNesting msg ->
    "Impredicative nesting prevents strict positivity: " ++ msg

-- ============================================================================
-- POSITION TRACKING
-- ============================================================================

{-| Polarity/position in type structure. -}
data Position
  = Positive      -- ^ Positive position (e.g., constructor argument)
  | Negative      -- ^ Negative position (e.g., function domain)
  deriving (Eq, Show, Generic)

{-| Flip polarity when entering negative context. -}
flipPos :: Position -> Position
flipPos Positive = Negative
flipPos Negative = Positive

{-| A type occurrence in a type definition. -}
data TypeOccurrence = TypeOccurrence
  { occVar      :: Text       -- ^ The type variable
  , occPosition :: Position   -- ^ Where it appears
  , occContext  :: String     -- ^ Contextual explanation
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- POSITIVITY CHECKING INTERFACE
-- ============================================================================

{-| Check if a type definition needs positivity checking.
Only inductive type definitions (data types) need checking.
-}
needsPositivityCheck :: Type -> Bool
needsPositivityCheck = \case
  TyConst _ _ -> True   -- Constructor type definition
  TyForall _ t -> needsPositivityCheck t
  _ -> False

{-| Check positivity of a single type definition.

Verifies that all type parameters appear only in strictly positive positions.

Returns:
- Left PosError if positivity is violated
- Right () if the type is strictly positive
-}
checkPositivity :: Text -> [Var] -> [Type] -> Either PosError ()
checkPositivity _typeName params constructorTypes = do
  let paramNames = Set.fromList [varName p | p <- params]
  mapM_ (\cty -> checkConstructorType paramNames cty) constructorTypes

{-| Check positivity of a constructor type argument. -}
checkConstructorType :: Set Text -> Type -> Either PosError ()
checkConstructorType paramNames ty =
  case analyzeTypeOccurrences paramNames ty of
    [] -> Right ()
    occs -> do
      let negativeOccs = [o | o <- occs, occPosition o == Negative]
      case negativeOccs of
        [] -> Right ()
        (occ:_) ->
          Left $ NonPositiveOccurrence
            (occVar occ)
            ("in " ++ occContext occ)

{-| Check positivity for multiple type definitions (e.g., mutual inductive types). -}
checkPositivityMulti :: [(Text, [Var], [Type])] -> Either PosError ()
checkPositivityMulti defs =
  mapM_ (\(name, params, ctors) -> checkPositivity name params ctors) defs

-- ============================================================================
-- OCCURRENCE ANALYSIS
-- ============================================================================

{-| Analyze all occurrences of type parameters in a type. -}
analyzeTypeOccurrences :: Set Text -> Type -> [TypeOccurrence]
analyzeTypeOccurrences params ty =
  analyzeType params ty Positive

{-| Recursively analyze a type, tracking position. -}
analyzeType :: Set Text -> Type -> Position -> [TypeOccurrence]
analyzeType params ty pos = go ty
  where
    go = \case
      TyVar (Var name _) ->
        if Set.member name params
          then [TypeOccurrence name pos (describePosition pos)]
          else []

      TyConst _qname args ->
        -- Type constructor arguments are positive
        concatMap (\arg -> analyzeType params arg Positive) args

      TyFun domain codomain ->
        -- Function domain is negative position (flip polarity)
        -- Function codomain is positive
        let domain_occs = analyzeType params domain (flipPos pos)
            codomain_occs = analyzeType params codomain pos
        in domain_occs ++ codomain_occs

      TyForall (Binder (Var bvar _) mbty) rest ->
        -- Skip analysis of bound variable
        let bty_occs = case mbty of
              Nothing -> []
              Just bty -> analyzeType params bty Positive
            rest_occs = analyzeType params rest pos
        in bty_occs ++ rest_occs

      TyUniverse _ -> []

      TyApp func arg ->
        -- Application: argument is typically positive
        analyzeType params func pos ++
        analyzeType params arg Positive

      TyEq e1 e2 ->
        -- Equality type contains terms, not type parameters
        -- Type parameters in terms are problematic
        analyzeTypeInTerms params e1 ++
        analyzeTypeInTerms params e2

{-| Analyze type parameters appearing in terms (rare, problematic). -}
analyzeTypeInTerms :: Set Text -> Term -> [TypeOccurrence]
analyzeTypeInTerms params term =
  case findTypeVarsInTerm params term of
    [] -> []
    vars -> [TypeOccurrence v Negative "in term equality" | v <- vars]

{-| Find type variable names in a term (as variables).
This is a heuristic - in a real type system, type parameters shouldn't appear in terms.
-}
findTypeVarsInTerm :: Set Text -> Term -> [Text]
findTypeVarsInTerm params = go
  where
    go = \case
      Var (Var name _) ->
        if Set.member name params then [name] else []
      Const _ -> []
      Lam _binder body -> go body
      App f x -> go f ++ go x
      Constr _qname args -> concatMap go args
      Case scrutinee clauses def ->
        go scrutinee ++
        concatMap (\(Clause _pat body) -> go body) clauses ++
        maybe [] go def
      Let _binder e1 e2 -> go e1 ++ go e2
      Ann e _ty -> go e
      Forall _binder body -> go body
      ProofTerm _p -> []
      Prop _prop -> []

{-| Describe the position in a human-readable way. -}
describePosition :: Position -> String
describePosition = \case
  Positive -> "positive position (constructor argument)"
  Negative -> "negative position (function domain)"

-- ============================================================================
-- STRICT POSITIVITY CHECKING (advanced)
-- ============================================================================

{-| Check strict positivity: type parameter appears only in positive positions
within constructor arguments.

This is more restrictive than just positivity - it prevents recursive definitions
that could be inconsistent.
-}
checkStrictPositivity :: Set Text -> Type -> Either PosError ()
checkStrictPositivity params ty = do
  let occs = analyzeTypeOccurrences params ty
  case [o | o <- occs, occPosition o == Negative] of
    [] -> Right ()
    (neg:_) ->
      Left $ TypeVarInFunctionDomain (occVar neg)

-- ============================================================================
-- UTILITIES
-- ============================================================================

{-| Check if a type parameter occurs in a type (occurs check). -}
typeParamOccurs :: Text -> Type -> Bool
typeParamOccurs param ty = go ty
  where
    go = \case
      TyVar (Var name _) -> name == param
      TyConst _qname args -> any go args
      TyFun domain codomain -> go domain || go codomain
      TyForall (Binder (Var _bvar _) mbty) rest ->
        case mbty of
          Nothing -> go rest
          Just bty -> go bty || go rest
      TyUniverse _ -> False
      TyApp func arg -> go func || go arg
      TyEq e1 e2 -> paramInTerm param e1 || paramInTerm param e2

{-| Check if a type parameter (viewed as a variable) occurs in a term. -}
paramInTerm :: Text -> Term -> Bool
paramInTerm param = go
  where
    go = \case
      Var (Var name _) -> name == param
      Const _ -> False
      Lam _binder body -> go body
      App f x -> go f || go x
      Constr _qname args -> any go args
      Case scrutinee clauses def ->
        go scrutinee ||
        any (\(Clause _pat body) -> go body) clauses ||
        maybe False go def
      Let _binder e1 e2 -> go e1 || go e2
      Ann e _ty -> go e
      Forall _binder body -> go body
      ProofTerm _p -> False
      Prop _prop -> False
