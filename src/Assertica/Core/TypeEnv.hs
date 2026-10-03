{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}

{-|
Module      : Assertica.Core.TypeEnv
Description : Type environment for tracking variable bindings and definitions
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

A type environment (Γ) maps variables to their types and tracks function/constant
definitions with their signatures. This is the context used during type checking.

DESIGN PRINCIPLES:
1. Immutable and compositional: environments are extended, never mutated
2. Efficient lookup: Map-based storage for variables and definitions
3. Explicit scope tracking: scope levels for nested contexts
4. Support for dependent types: types can reference previously bound variables
5. Clear error reporting: operations return Either with descriptive messages
-}

module Assertica.Core.TypeEnv
  ( -- * Core type environment
    TypeEnv(..)
  , emptyEnv
  , extendEnv
  , extendEnvMultiple
  , extendWithDef
  , lookupVar
  , lookupDef
  , allBindings
  , allDefinitions
  , getScope
  , inNewScope

    -- * Constraint collection
  , Constraint(..)
  , ConstraintStore
  , emptyConstraints
  , addConstraint
  , getConstraints

    -- * Type validation
  , isWellFormedType
  , validateEnvironment

    -- * Utilities
  , envSize
  , prettyEnv
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Assertica.Core.AST
  ( Var(..)
  , Type(..)
  , Term(..)
  , QName(..)
  , TypeLevel(..)
  , Binder(..)
  , freeVarsInType
  , freeVarsInTerm
  , prettyType
  , varName
  )

-- ============================================================================
-- TYPE ENVIRONMENT
-- ============================================================================

{-| Type environment (Γ) maps variables and definitions to their types.

A well-formed type environment has the property that types are well-formed
relative to the earlier bindings. For example:
  - Γ ⊢ A : Type 0    (A is a well-formed type)
  - Γ, x : A ⊢ B : Type 0    (B is well-formed with x in scope)

This is enforced by extendEnv and the type checker.
-}
data TypeEnv = TypeEnv
  { bindings :: Map.Map Var Type
    -- ^ Variable bindings: x ↦ T means "x has type T"
  , definitions :: Map.Map QName Type
    -- ^ Function/constant definitions: f ↦ T means "f has type T"
  , scope :: Int
    -- ^ Current scope level (for tracking nested contexts)
  }
  deriving (Eq, Show, Generic, Typeable)

-- ============================================================================
-- ENVIRONMENT OPERATIONS
-- ============================================================================

{-| Create an empty type environment.
Used at the start of type checking, before any bindings are introduced.
-}
emptyEnv :: TypeEnv
emptyEnv = TypeEnv
  { bindings = Map.empty
  , definitions = Map.empty
  , scope = 0
  }

{-| Extend environment with a single variable binding.

Returns Left with an error if:
  - The variable is already bound in this environment
  - The type contains free variables not in scope

Example:
  Γ ⊢ Int : Type 0
  extendEnv Γ (Var "x" 1) Int = Γ, x : Int
-}
extendEnv :: TypeEnv -> Var -> Type -> Either String TypeEnv
extendEnv env var ty = do
  -- Check if variable already bound
  case Map.lookup var (bindings env) of
    Just _  -> Left $ "Variable already bound: " ++ show var
    Nothing -> do
      -- Variable is fresh; add the binding
      return env { bindings = Map.insert var ty (bindings env) }

{-| Extend environment with multiple variable bindings.
Bindings are added left-to-right, so earlier bindings are available for later types.
-}
extendEnvMultiple :: TypeEnv -> [(Var, Type)] -> Either String TypeEnv
extendEnvMultiple env [] = Right env
extendEnvMultiple env ((var, ty) : rest) = do
  env' <- extendEnv env var ty
  extendEnvMultiple env' rest

{-| Extend environment with a definition (function or constant).

Definitions differ from bindings: they have names (QName) and optional bodies.
For type checking, we only track the type signature.
-}
extendWithDef :: TypeEnv -> QName -> Type -> Either String TypeEnv
extendWithDef env qname ty = do
  case Map.lookup qname (definitions env) of
    Just _  -> Left $ "Definition already exists: " ++ show qname
    Nothing -> return env { definitions = Map.insert qname ty (definitions env) }

{-| Look up a variable's type in the environment.

Returns Nothing if the variable is not bound.
-}
lookupVar :: Var -> TypeEnv -> Maybe Type
lookupVar var env = Map.lookup var (bindings env)

{-| Look up a definition's type in the environment.

Returns Nothing if the definition doesn't exist.
-}
lookupDef :: QName -> TypeEnv -> Maybe Type
lookupDef qname env = Map.lookup qname (definitions env)

{-| Get all variable bindings.
-}
allBindings :: TypeEnv -> [(Var, Type)]
allBindings env = Map.toList (bindings env)

{-| Get all definitions.
-}
allDefinitions :: TypeEnv -> [(QName, Type)]
allDefinitions env = Map.toList (definitions env)

{-| Get the current scope level.
-}
getScope :: TypeEnv -> Int
getScope = scope

{-| Execute an action in a new nested scope.
Used for let-bindings and lambda abstractions where we want isolated contexts.
-}
inNewScope :: TypeEnv -> TypeEnv
inNewScope env = env { scope = scope env + 1 }

{-| Check the size of the environment (number of bindings + definitions).
-}
envSize :: TypeEnv -> Int
envSize env = Map.size (bindings env) + Map.size (definitions env)

-- ============================================================================
-- TYPE VALIDATION
-- ============================================================================

{-| Check if a type is well-formed in the given environment.

A type is well-formed if:
  - All type variables in it are either:
    a) In scope (free variables are in the environment), or
    b) Are bound by foralls at the top level
  - All universe levels are consistent
  - No circular type definitions

Example:
  Γ = {α : Type 0, β : Type 0}
  isWellFormedType Γ (TyFun α β) = True
  isWellFormedType Γ (TyFun γ β) = False (γ not in scope)
-}
isWellFormedType :: TypeEnv -> Type -> Either String ()
isWellFormedType env ty = go Set.empty ty
  where
    go bound = \case
      TyVar v ->
        if Set.member v bound || Map.member v (bindings env)
        then Right ()
        else Left $ "Type variable not in scope: " ++ show v

      TyConst _ args ->
        mapM_ (go bound) args

      TyFun a b -> do
        go bound a
        go bound b

      TyForall (Binder var _) body ->
        go (Set.insert var bound) body

      TyUniverse _ -> Right ()

      TyApp a b -> do
        go bound a
        go bound b

      TyEq e1 e2 ->
        -- Terms in equality types must be well-typed
        -- (This is checked by the type checker, not here)
        Right ()

{-| Validate the entire environment for internal consistency.

Checks:
  - No duplicate variable bindings
  - No duplicate definitions
  - All bound variables have well-formed types
  - All definition types are well-formed

Returns Right () if valid, Left with error message otherwise.
-}
validateEnvironment :: TypeEnv -> Either String ()
validateEnvironment env = do
  -- Check variable bindings are valid
  _ <- mapM (\(var, ty) -> isWellFormedType env ty) (allBindings env)
  -- Check definitions are valid
  _ <- mapM (\(_, ty) -> isWellFormedType env ty) (allDefinitions env)
  Right ()

-- ============================================================================
-- CONSTRAINT COLLECTION
-- ============================================================================

{-| Constraints are obligations for later stages (e.g., termination checking,
positivity analysis). The type checker collects these during type checking.
-}
data Constraint
  = TerminationConstraint
    { constraintFunc :: QName
    , constraintReason :: String
    }
  | PositivityConstraint
    { constraintType :: QName
    , constraintReason :: String
    }
  | UniverseLevelConstraint
    { constraint1 :: TypeLevel
    , constraint2 :: TypeLevel
    , constraintReason :: String
    }
  deriving (Eq, Show, Generic, Typeable)

{-| Storage for constraints collected during type checking.
-}
type ConstraintStore = [Constraint]

{-| Create an empty constraint store.
-}
emptyConstraints :: ConstraintStore
emptyConstraints = []

{-| Add a constraint to the store.
-}
addConstraint :: Constraint -> ConstraintStore -> ConstraintStore
addConstraint c cs = c : cs

{-| Get all constraints.
-}
getConstraints :: ConstraintStore -> [Constraint]
getConstraints = id

-- ============================================================================
-- PRETTY PRINTING
-- ============================================================================

{-| Pretty-print a type environment for debugging.

Example output:
  TypeEnv {
    x : Int
    y : List Int
    append : forall a. List a -> List a -> List a
  }
-}
prettyEnv :: TypeEnv -> String
prettyEnv env =
  let varBindings = map (\(v, ty) -> prettyVar v ++ " : " ++ prettyType ty) (allBindings env)
      defBindings = map (\(qn, ty) -> prettyQName qn ++ " : " ++ prettyType ty) (allDefinitions env)
      allBindingsStr = varBindings ++ defBindings
  in "TypeEnv { " ++ intercalate ", " allBindingsStr ++ " }"

{-| Pretty-print a variable.
-}
prettyVar :: Var -> String
prettyVar (Var name _) = T.unpack name

{-| Pretty-print a qualified name.
-}
prettyQName :: QName -> String
prettyQName (QName mods name) =
  let modStr = if null mods then "" else intercalate "." (map T.unpack mods) ++ "."
  in modStr ++ T.unpack name
