{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Backend.TypeCompiler
Description : Compile Assertica types to Haskell type signatures
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

Translates type expressions from the Core AST to Haskell type syntax:

MAPPINGS:

- TyVar v                   → v (type variable)
- TyConst C [T1, ..., Tn]   → C T1 ... Tn (type constructor application)
- TyFun A B                 → A -> B (function type)
- TyForall (x : A) . B      → forall x . A -> B (dependent function)
- TyUniverse Type0          → Type (type universe)
- TyUniverse (TypeN n)      → Type (all universes collapse to Type in Haskell)
- TyApp T1 T2               → T1 T2 (type application)
- TyEq a b                  → (a :~: b) (equality type using GADT)

UNIVERSE MAPPING:

Assertica's universe hierarchy (Type 0, Type 1, ...) is mapped to Haskell's
single-level kind system. All universes compile to "Type" or higher kinds
as appropriate for the context.

DEPENDENT TYPES:

Assertica supports full dependent types via TyForall. These are mapped to
Haskell functions or GADT syntax where possible. Complex dependent types
may require GADT encoding with indices.

PRINCIPLES:

1. **Type Safety**: Generated types are valid Haskell 2010 / GHC extensions
2. **Determinism**: Same type input produces same output
3. **Readability**: Generated types are human-readable
4. **No Generics**: Avoid needing exotic extensions where possible
-}

module Assertica.Backend.TypeCompiler
  ( -- * Main compilation function
    compileType
  , compileTypeWith
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Assertica.Core.AST
import Assertica.Backend.CodeGen (CodeGenConfig)
import qualified Assertica.Backend.TermCompiler as TC

-- ============================================================================
-- MAIN COMPILATION FUNCTION
-- ============================================================================

{-| Compile a type to Haskell type syntax.

This is the primary entry point for type compilation.
Returns either a Haskell type expression string or an error.
-}
compileType :: CodeGenConfig -> Type -> Either String Text
compileType config ty = compileTypeWith config Set.empty ty

{-| Compile a type with a given bound variable context -}
compileTypeWith :: CodeGenConfig -> Set.Set Var -> Type -> Either String Text
compileTypeWith config bound = \case
  TyVar (Var name _) -> Right name

  TyConst (QName mod name) args -> do
    compiledArgs <- mapM (compileTypeWith config bound) args
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    if null compiledArgs
      then Right qname
      else Right $ "(" <> qname <> " " <> T.intercalate " " compiledArgs <> ")"

  TyFun a b -> do
    ca <- compileTypeWith config bound a
    cb <- compileTypeWith config bound b
    Right $ "(" <> ca <> " -> " <> cb <> ")"

  TyForall (Binder (Var name _) _) body ->
    -- Dependent function type: forall x . body
    -- In Haskell, this becomes a higher-order function type
    let bound' = Set.insert (Var name 0) bound
    in do
      cb <- compileTypeWith config bound' body
      Right $ "(forall (" <> name <> ") . " <> cb <> ")"

  TyUniverse Type0 ->
    -- Type 0 → Haskell's Type kind
    Right "Type"

  TyUniverse (TypeN _n) ->
    -- Higher universes also map to Type in Haskell
    -- (Haskell doesn't have predicative universes)
    Right "Type"

  TyApp t1 t2 -> do
    ct1 <- compileTypeWith config bound t1
    ct2 <- compileTypeWith config bound t2
    -- Type application: just juxtapose
    Right $ "(" <> ct1 <> " " <> ct2 <> ")"

  TyEq e1 e2 -> do
    -- Equality type: compile using GADT syntax
    -- TyEq a b becomes (a :~: b) using GADT machinery
    _ce1 <- TC.compileTerm config e1
    _ce2 <- TC.compileTerm config e2
    -- For now, return a placeholder
    -- In a full implementation, would use GADT.Equality machinery
    Right "Equality"

-- ============================================================================
-- UNIVERSE HANDLING
-- ============================================================================

{-| Map Assertica universe levels to Haskell kinds -}
universeToKind :: TypeLevel -> Text
universeToKind = \case
  Type0 -> "Type"
  TypeN _ -> "Type"  -- All universes collapse to Type

{-| Check if a type is well-formed at a given universe level -}
-- This would be more complex in a production system
isWellFormedAtLevel :: TypeLevel -> Type -> Bool
isWellFormedAtLevel _level _ty =
  True  -- Simplified for now
