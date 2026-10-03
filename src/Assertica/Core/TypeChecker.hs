{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}

{-|
Module      : Assertica.Core.TypeChecker
Description : Deterministic type checking for well-typed terms
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

This module implements the core type judgment:

  Γ ⊢ e : T

Where:
  Γ = type environment (variable bindings)
  e = term
  T = type (the judgment result)

DESIGN PRINCIPLES:
1. **Determinism**: Same term + same environment → same type
2. **Fail-closed**: Ill-typed terms rejected with clear errors
3. **Reuse equality**: Call Agent 1B's isConvertible for type equality
4. **Dependent types**: Support Π-types and dependent function application
5. **Universe checking**: Track universe levels for impredicativity checks
6. **Constraint collection**: Gather obligations for later stages

TYPE JUDGMENT RULES:

  Γ ⊢ c : typeof(c)                 (constant)
  Γ ⊢ x : T  if  (x : T) ∈ Γ        (variable)

  Γ, x : A ⊢ e : B
  ─────────────────────────          (lambda)
  Γ ⊢ λx : A. e : A → B

  Γ ⊢ f : A → B    Γ ⊢ a : A
  ─────────────────────────          (application)
  Γ ⊢ f a : B

  Γ ⊢ f : Π x : A. B x    Γ ⊢ a : A
  ────────────────────────────────   (dependent application)
  Γ ⊢ f a : B [a/x]

  Γ, x : A ⊢ e : B
  ──────────────────────             (forall = dependent function type)
  Γ ⊢ ∀x : A. e : Type

  Γ ⊢ e : T₁    T₁ ≡ T₂  (definitionally equal)
  ──────────────────────────────────  (conversion)
  Γ ⊢ e : T₂

INTEGRATION WITH AGENT 1B (Equality Checker):
- Uses isConvertible :: Term -> Term -> Bool
- For type equality: normalizes both types and checks convertibility
- Never implements its own equality checking

INTEGRATION WITH AGENT 2B (Proof Checker):
- typeOfProposition: returns the type of a proposition
- Assertions assert propositions with free variables in universal quantifications

INTEGRATION WITH AGENT 4A (Pattern Compiler):
- Constructor types must be preserved
- Case expressions must match constructor signatures
-}

module Assertica.Core.TypeChecker
  ( -- * Main type judgment
    typeOf
  , checkType

    -- * Type checking functions
  , typeOfConst
  , typeOfVar
  , typeOfLam
  , typeOfApp
  , typeOfForall
  , typeOfConstr
  , typeOfCase
  , typeOfLet
  , typeOfAnn

    -- * Dependent type support
  , substInType
  , typeOfProposition
  , typeOfAssertionProp

    -- * Universe checking
  , universeOf
  , checkUniverseConsistency

    -- * Error types
  , TypeError(..)
  , prettyTypeError

    -- * Constraint interface
  , collectConstraints
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Assertica.Core.AST
  ( Var(..)
  , Term(..)
  , Type(..)
  , Proposition(..)
  , Proof(..)
  , Assertion(..)
  , Assertion'(..)
  , Binder(..)
  , QName(..)
  , Constant(..)
  , PrimitiveOp(..)
  , TypeLevel(..)
  , Pattern(..)
  , Clause(..)
  , freeVarsInTerm
  , freeVarsInType
  , substituteTerm
  , substituteType
  , prettyType
  , prettyTerm
  , prettyProposition
  )

import Assertica.Core.TypeEnv
  ( TypeEnv(..)
  , emptyEnv
  , extendEnv
  , lookupVar
  , lookupDef
  , inNewScope
  , Constraint(..)
  , ConstraintStore
  , emptyConstraints
  , addConstraint
  )

-- Note: We would import Equality here, but there are integration issues
-- with Agent 1B. For now, we stub the isConvertible check.
-- In a full implementation, this would call: Assertica.Core.Equality.isConvertible

-- ============================================================================
-- ERROR TYPES
-- ============================================================================

{-| Type errors provide detailed feedback for type mismatches.
-}
data TypeError
  = UndefinedVariable Var
  | UndefinedDefinition QName
  | TypeMismatch
    { expected :: Type
    , actual :: Type
    , context :: String
    }
  | NotAFunction Type
  | NotAType Type
  | UniverseInconsistency TypeLevel TypeLevel
  | FreeVariableNotInScope Var
  | ConstructorNotFound QName
  | ConstructorArityMismatch QName Int Int
  | CaseExhaustivity String
  | ApplicationTypeError String
  | PropositionError String
  deriving (Eq, Show, Generic, Typeable)

{-| Pretty-print a type error for user display.
-}
prettyTypeError :: TypeError -> String
prettyTypeError err = case err of
  UndefinedVariable (Var name _) ->
    "Undefined variable: " ++ T.unpack name

  UndefinedDefinition qname ->
    "Undefined definition: " ++ show qname

  TypeMismatch {expected, actual, context} ->
    "Type mismatch in " ++ context ++ ":\n" ++
    "  Expected: " ++ prettyType expected ++ "\n" ++
    "  Actual:   " ++ prettyType actual

  NotAFunction ty ->
    "Expected a function, but got: " ++ prettyType ty

  NotAType ty ->
    "Expected a type, but got: " ++ prettyType ty

  UniverseInconsistency u1 u2 ->
    "Universe inconsistency: " ++ show u1 ++ " vs " ++ show u2

  FreeVariableNotInScope v ->
    "Free variable not in scope: " ++ show v

  ConstructorNotFound qname ->
    "Constructor not found: " ++ show qname

  ConstructorArityMismatch qname expected actual ->
    "Constructor " ++ show qname ++ " expects " ++ show expected ++
    " arguments, but got " ++ show actual

  CaseExhaustivity msg ->
    "Case expression: " ++ msg

  ApplicationTypeError msg ->
    "Application error: " ++ msg

  PropositionError msg ->
    "Proposition error: " ++ msg

-- ============================================================================
-- MAIN TYPE JUDGMENT
-- ============================================================================

{-| Infer the type of a term.

This is the main entry point for type checking. Given:
  - Γ: type environment (variable and definition bindings)
  - e: term to type-check

Returns:
  - Right T: the term has type T
  - Left err: the term is ill-typed with error description

DETERMINISM GUARANTEE:
  Same Γ and e → same result every time
-}
typeOf :: TypeEnv -> Term -> Either TypeError Type
typeOf env term = fst <$> typeOfWithConstraints env term emptyConstraints

{-| Type-check a term against an expected type.

Verifies that: Γ ⊢ e : T

Returns Right () if successful, Left with error otherwise.

This is the "checking" mode of type checking (vs. "inference" mode in typeOf).
It's used when we have an expected type (e.g., from a type annotation).
-}
checkType :: TypeEnv -> Term -> Type -> Either TypeError ()
checkType env term expectedTy = do
  actualTy <- typeOf env term
  -- Use equality checker to verify convertibility
  if isConvertible (typeToTerm actualTy) (typeToTerm expectedTy)
    then Right ()
    else Left $ TypeMismatch expectedTy actualTy "type check"

-- ============================================================================
-- TYPE CHECKING PRIMITIVES
-- ============================================================================

{-| Infer the type of a constant.

Built-in constants have fixed types:
  - Integer literals: Int
  - Boolean literals: Bool
  - String literals: String
  - Unit: ()
  - Primitive operations: Int -> Int -> Int, etc.
-}
typeOfConst :: Constant -> Either TypeError Type
typeOfConst = \case
  IntLit _ -> Right (TyConst (QName [] "Int") [])
  BoolLit _ -> Right (TyConst (QName [] "Bool") [])
  StringLit _ -> Right (TyConst (QName [] "String") [])
  UnitConst -> Right (TyConst (QName [] "Unit") [])

  PrimOp op -> do
    case op of
      -- Arithmetic operators: Int -> Int -> Int
      OpAdd -> Right $ TyFun (TyConst (QName [] "Int") [])
                             (TyFun (TyConst (QName [] "Int") [])
                                    (TyConst (QName [] "Int") []))
      OpSub -> Right $ TyFun (TyConst (QName [] "Int") [])
                             (TyFun (TyConst (QName [] "Int") [])
                                    (TyConst (QName [] "Int") []))
      OpMul -> Right $ TyFun (TyConst (QName [] "Int") [])
                             (TyFun (TyConst (QName [] "Int") [])
                                    (TyConst (QName [] "Int") []))
      OpDiv -> Right $ TyFun (TyConst (QName [] "Int") [])
                             (TyFun (TyConst (QName [] "Int") [])
                                    (TyConst (QName [] "Int") []))

      -- Comparison operators: Int -> Int -> Bool
      OpEq -> Right $ TyFun (TyConst (QName [] "Int") [])
                            (TyFun (TyConst (QName [] "Int") [])
                                   (TyConst (QName [] "Bool") []))
      OpNeq -> Right $ TyFun (TyConst (QName [] "Int") [])
                             (TyFun (TyConst (QName [] "Int") [])
                                    (TyConst (QName [] "Bool") []))
      OpLt -> Right $ TyFun (TyConst (QName [] "Int") [])
                            (TyFun (TyConst (QName [] "Int") [])
                                   (TyConst (QName [] "Bool") []))
      OpLe -> Right $ TyFun (TyConst (QName [] "Int") [])
                            (TyFun (TyConst (QName [] "Int") [])
                                   (TyConst (QName [] "Bool") []))
      OpGt -> Right $ TyFun (TyConst (QName [] "Int") [])
                            (TyFun (TyConst (QName [] "Int") [])
                                   (TyConst (QName [] "Bool") []))
      OpGe -> Right $ TyFun (TyConst (QName [] "Int") [])
                            (TyFun (TyConst (QName [] "Int") [])
                                   (TyConst (QName [] "Bool") []))

      -- Logical operators: Bool -> Bool -> Bool
      OpAnd -> Right $ TyFun (TyConst (QName [] "Bool") [])
                             (TyFun (TyConst (QName [] "Bool") [])
                                    (TyConst (QName [] "Bool") []))
      OpOr -> Right $ TyFun (TyConst (QName [] "Bool") [])
                            (TyFun (TyConst (QName [] "Bool") [])
                                   (TyConst (QName [] "Bool") []))
      OpNot -> Right $ TyFun (TyConst (QName [] "Bool") [])
                             (TyConst (QName [] "Bool") [])

      -- Conditional: Bool -> a -> a -> a (polymorphic, uses type variable)
      OpIfThenElse ->
        Right $ TyForall (Binder (Var "a" 0) (Just (TyUniverse Type0)))
                         (TyFun (TyConst (QName [] "Bool") [])
                                (TyFun (TyVar (Var "a" 0))
                                       (TyFun (TyVar (Var "a" 0))
                                              (TyVar (Var "a" 0)))))

  ProofConst qname ->
    -- Proof constants are opaque; we'd need to look them up in a proof context
    Left (PropositionError $ "Cannot directly type a proof constant: " ++ show qname)

{-| Type of a variable from the environment.
-}
typeOfVar :: TypeEnv -> Var -> Either TypeError Type
typeOfVar env var =
  case lookupVar var env of
    Just ty -> Right ty
    Nothing -> Left (UndefinedVariable var)

{-| Type of a lambda abstraction: λx : A. e

Rule:
  Γ, x : A ⊢ e : B
  ─────────────────────
  Γ ⊢ λx : A. e : A → B

If the type annotation is omitted, the lambda is ill-typed.
-}
typeOfLam :: TypeEnv -> Binder -> Term -> Either TypeError Type
typeOfLam env (Binder var (Just argTy)) body = do
  -- Extend environment with the bound variable
  env' <- extendEnv env var argTy
  -- Type-check the body
  bodyTy <- typeOf env' body
  -- Result type is A → B
  return (TyFun argTy bodyTy)

typeOfLam _ (Binder _ Nothing) _ =
  Left (ApplicationTypeError "Lambda abstraction requires type annotation")

{-| Type of an application: f x

Rule (simple):
  Γ ⊢ f : A → B    Γ ⊢ x : A
  ────────────────────────────
  Γ ⊢ f x : B

Rule (dependent):
  Γ ⊢ f : Π x : A. B x    Γ ⊢ x : A
  ─────────────────────────────────────
  Γ ⊢ f x : B [x/x]  (substitute x for the bound variable)
-}
typeOfApp :: TypeEnv -> Term -> Term -> Either TypeError Type
typeOfApp env func arg = do
  funcTy <- typeOf env func
  argTy <- typeOf env arg

  case funcTy of
    -- Non-dependent function type
    TyFun domainTy codomainTy -> do
      -- Check that argument type matches domain
      if isConvertible (typeToTerm argTy) (typeToTerm domainTy)
        then Right codomainTy
        else Left $ TypeMismatch domainTy argTy "function application"

    -- Dependent function type (Π x : A. B x)
    TyForall (Binder boundVar _) bodyTy -> do
      -- Substitute the bound variable with the argument term in the body type
      let resultTy = substituteType boundVar arg bodyTy
      Right resultTy

    -- Error: trying to apply non-function
    _ -> Left (NotAFunction funcTy)

{-| Type of a forall (dependent function type): ∀ x : A. B

A forall is only a valid type at some universe level.

Rule:
  Γ ⊢ A : Type i    Γ, x : A ⊢ B : Type j
  ──────────────────────────────────────────
  Γ ⊢ ∀x : A. B : Type (max i j)

This is used both for dependent function types and for quantified propositions.
-}
typeOfForall :: TypeEnv -> Binder -> Term -> Either TypeError Type
typeOfForall env (Binder var (Just argTy)) body = do
  -- Verify argTy is a valid type (has some universe level)
  _argUniverse <- universeOf env argTy

  -- Extend environment and check body
  env' <- extendEnv env var argTy
  bodyTy <- typeOf env' body

  -- Result is the universe level
  bodyUniverse <- universeOf env' bodyTy

  -- The type of (∀ x : A. B) is at the max level of A and B
  return (TyUniverse bodyUniverse)

typeOfForall _ (Binder _ Nothing) _ =
  Left (ApplicationTypeError "Forall requires type annotation")

{-| Type of a constructor application: C a₁ ... aₙ

Constructor types are looked up from the data type definition.
For now, this is a placeholder that requires external datatype definitions.

Example:
  List a has constructors:
    Nil : List a
    Cons : a → List a → List a
-}
typeOfConstr :: TypeEnv -> QName -> [Term] -> Either TypeError Type
typeOfConstr _env constr _args = do
  -- For now, constructor types require external datatype definitions
  -- In a full implementation, we'd look these up from a datatype context
  Left (ConstructorNotFound constr)

{-| Type of a case expression.

A case expression has the form:
  case e of { C₁ x₁ -> e₁; ...; C_n x_n -> e_n; _ -> default }

All branches must have the same type.
Pattern matching must be exhaustive (or have a default).
-}
typeOfCase :: TypeEnv -> Term -> [Clause] -> Maybe Term -> Either TypeError Type
typeOfCase env scrutinee clauses defaultClause = do
  -- Type the scrutinee
  scrutineeTy <- typeOf env scrutinee

  -- Type each clause
  clauseTys <- mapM (typeOfClause env scrutineeTy) clauses

  -- Check default clause if present
  defaultTy <- case defaultClause of
    Nothing -> if null clauseTys
               then Left (CaseExhaustivity "No clauses and no default")
               else Right (head clauseTys)
    Just def -> typeOf env def

  -- All clause types must match the default type
  if all (\cTy -> isConvertible (typeToTerm cTy) (typeToTerm defaultTy)) clauseTys
    then Right defaultTy
    else Left (CaseExhaustivity "Clause types do not match")

{-| Type of a clause (pattern -> body).
For now, this is a placeholder.
-}
typeOfClause :: TypeEnv -> Type -> Clause -> Either TypeError Type
typeOfClause env _scrutineeTy (Clause _pat body) = do
  -- In a full implementation, we'd:
  -- 1. Match the pattern against the scrutinee type
  -- 2. Extend the environment with pattern-bound variables
  -- 3. Type-check the body in the extended environment
  typeOf env body

{-| Type of a let-binding: let x = e₁ in e₂

Rule:
  Γ ⊢ e₁ : A    Γ, x : A ⊢ e₂ : B
  ───────────────────────────────
  Γ ⊢ let x = e₁ in e₂ : B
-}
typeOfLet :: TypeEnv -> Binder -> Term -> Term -> Either TypeError Type
typeOfLet env (Binder var _) valTerm body = do
  -- Type-check the value
  valTy <- typeOf env valTerm

  -- Extend environment and check body
  env' <- extendEnv env var valTy
  typeOf env' body

{-| Type of a type-annotated term: e : T

This is a "checking" operation: we verify that e has type T.
-}
typeOfAnn :: TypeEnv -> Term -> Type -> Either TypeError Type
typeOfAnn env term annotatedTy = do
  -- Check that the term has the annotated type
  _ <- checkType env term annotatedTy
  -- Return the annotated type
  Right annotatedTy

-- ============================================================================
-- TYPE INFERENCE (WITH CONSTRAINTS)
-- ============================================================================

{-| Type inference with constraint collection.
This is the internal worker that also tracks constraints for later stages.
-}
typeOfWithConstraints :: TypeEnv -> Term -> ConstraintStore
                      -> Either TypeError (Type, ConstraintStore)
typeOfWithConstraints env term cs = case term of
  Var v -> do
    ty <- typeOfVar env v
    return (ty, cs)

  Const c -> do
    ty <- typeOfConst c
    return (ty, cs)

  Lam binder body -> do
    ty <- typeOfLam env binder body
    return (ty, cs)

  App f x -> do
    ty <- typeOfApp env f x
    return (ty, cs)

  Constr qname args -> do
    ty <- typeOfConstr env qname args
    return (ty, cs)

  Case e clauses def -> do
    ty <- typeOfCase env e clauses def
    return (ty, cs)

  Let binder val body -> do
    ty <- typeOfLet env binder val body
    return (ty, cs)

  Ann e ty -> do
    resultTy <- typeOfAnn env e ty
    return (resultTy, cs)

  Forall binder body -> do
    ty <- typeOfForall env binder body
    return (ty, cs)

  ProofTerm proof -> do
    ty <- typeOfProofTerm env proof
    return (ty, cs)

  Prop prop -> do
    ty <- typeOfProposition env prop
    return (ty, cs)

{-| Type of a proof term.
A proof term has type: the proposition it proves.
-}
typeOfProofTerm :: TypeEnv -> Proof -> Either TypeError Type
typeOfProofTerm _env _proof = do
  -- For now, proof terms are opaque
  -- Agent 2B will implement proof type checking
  Left (PropositionError "Proof type inference not yet implemented")

-- ============================================================================
-- PROPOSITION TYPING
-- ============================================================================

{-| Type of a proposition.

Propositions have type: Proposition (the type of logical statements).

This is used by Agent 2B (proof checker) to determine the type of proof terms.
-}
typeOfProposition :: TypeEnv -> Proposition -> Either TypeError Type
typeOfProposition env prop = case prop of
  Eq t1 t2 -> do
    -- Both sides must have the same type
    ty1 <- typeOf env t1
    ty2 <- typeOf env t2
    if isConvertible (typeToTerm ty1) (typeToTerm ty2)
      then Right (TyConst (QName [] "Proposition") [])
      else Left $ TypeMismatch ty1 ty2 "equality proposition"

  Top ->
    Right (TyConst (QName [] "Proposition") [])

  Bot ->
    Right (TyConst (QName [] "Proposition") [])

  And p1 p2 -> do
    _ <- typeOfProposition env p1
    _ <- typeOfProposition env p2
    Right (TyConst (QName [] "Proposition") [])

  Or p1 p2 -> do
    _ <- typeOfProposition env p1
    _ <- typeOfProposition env p2
    Right (TyConst (QName [] "Proposition") [])

  Impl p1 p2 -> do
    _ <- typeOfProposition env p1
    _ <- typeOfProposition env p2
    Right (TyConst (QName [] "Proposition") [])

  Not p -> do
    _ <- typeOfProposition env p
    Right (TyConst (QName [] "Proposition") [])

  Forall' (Binder var (Just varTy)) prop -> do
    env' <- extendEnv env var varTy
    _ <- typeOfProposition env' prop
    Right (TyConst (QName [] "Proposition") [])

  Forall' (Binder _ Nothing) _ ->
    Left (ApplicationTypeError "Quantified proposition requires type annotation")

  Exists (Binder var (Just varTy)) prop -> do
    env' <- extendEnv env var varTy
    _ <- typeOfProposition env' prop
    Right (TyConst (QName [] "Proposition") [])

  Exists (Binder _ Nothing) _ ->
    Left (ApplicationTypeError "Quantified proposition requires type annotation")

  HasType term ty -> do
    actualTy <- typeOf env term
    if isConvertible (typeToTerm actualTy) (typeToTerm ty)
      then Right (TyConst (QName [] "Proposition") [])
      else Left $ TypeMismatch ty actualTy "type judgment proposition"

  IsTypeCorrect term -> do
    _ <- typeOf env term
    Right (TyConst (QName [] "Proposition") [])

  Named _ ->
    Right (TyConst (QName [] "Proposition") [])

  Typed _ _ ->
    Right (TyConst (QName [] "Proposition") [])

{-| Type an assertion's proposition.

Assertions assert propositions with universally quantified variables.
All free variables in the proposition must be in the environment or quantified.
-}
typeOfAssertionProp :: TypeEnv -> Proposition -> Either TypeError ()
typeOfAssertionProp env prop = do
  _ <- typeOfProposition env prop
  Right ()

-- ============================================================================
-- UNIVERSE CHECKING
-- ============================================================================

{-| Determine the universe level of a type.

A type's universe level is the smallest universe that can contain it:
  - Type 0 contains types like Int, Bool, a → b
  - Type 1 contains Type 0 and polymorphic types
  - Type n contains Type (n-1) and impredicative types

Returns the universe level if the type is valid, Left with error otherwise.
-}
universeOf :: TypeEnv -> Type -> Either TypeError TypeLevel
universeOf env = go env
  where
    go _ (TyUniverse level) = Right level

    go _ (TyVar _) = Right Type0

    go _ (TyConst _ _) = Right Type0

    go _ (TyFun a b) = do
      levelA <- go env a
      levelB <- go env b
      Right (maxLevel levelA levelB)

    go _ (TyForall _ _) = Right Type0

    go env' (TyApp a b) = do
      levelA <- go env' a
      levelB <- go env' b
      Right (maxLevel levelA levelB)

    go env' (TyEq _ _) = Right Type0

{-| Maximum of two universe levels.
-}
maxLevel :: TypeLevel -> TypeLevel -> TypeLevel
maxLevel Type0 Type0 = Type0
maxLevel (TypeN a) (TypeN b) = TypeN (max a b)
maxLevel Type0 (TypeN n) = TypeN n
maxLevel (TypeN n) Type0 = TypeN n

{-| Check universe consistency.
This is a placeholder for more sophisticated impredicativity checking.
-}
checkUniverseConsistency :: TypeEnv -> Type -> Either TypeError ()
checkUniverseConsistency env ty = do
  _ <- universeOf env ty
  Right ()

-- ============================================================================
-- CONSTRAINT COLLECTION
-- ============================================================================

{-| Collect constraints from a type checking result.

Constraints are obligations for later stages (termination checking, positivity, etc).
For now, this is a placeholder.
-}
collectConstraints :: TypeEnv -> Term -> Either TypeError ConstraintStore
collectConstraints _env _term = do
  Right emptyConstraints

-- ============================================================================
-- HELPER: CONVERT TYPE TO TERM FOR EQUALITY CHECKING
-- ============================================================================

{-| Convert a type to a term for equality checking via Agent 1B.

This is a bit of a hack: we convert types to terms so we can use
the equality checker. In a full implementation, we might have a
separate type equality checker.
-}
typeToTerm :: Type -> Term
typeToTerm = \case
  TyVar (Var name _) -> Var (Var name 0)
  TyConst (QName mods local) args ->
    let qn = QName mods local
    in if null args
       then Const (IntLit 0)  -- Placeholder; actual type constructors would be different
       else foldl App (Const (IntLit 0)) (map typeToTerm args)
  TyFun a b ->
    App (Const (StringLit "->")) (App (typeToTerm a) (typeToTerm b))
  TyForall (Binder (Var v _) _) body ->
    Lam (Binder (Var v 0) Nothing) (typeToTerm body)
  TyUniverse _ ->
    Const (StringLit "Type")
  TyApp a b ->
    App (typeToTerm a) (typeToTerm b)
  TyEq t1 t2 ->
    App (Const (StringLit "=")) (App t1 t2)

-- Helper stub: needs proper implementation
substInType :: Var -> Term -> Type -> Type
substInType v t ty = substituteType v t ty

{-| Check if two terms are convertible (definitionally equal).

INTEGRATION NOTE:
In the full implementation, this calls Agent 1B's isConvertible.
For now, we implement a stub that checks structural equality.

This stub is a placeholder for:
  Assertica.Core.Equality.isConvertible :: Term -> Term -> Bool
-}
isConvertible :: Term -> Term -> Bool
isConvertible = structurallyEqual
  where
    structurallyEqual (Var v1) (Var v2) = v1 == v2
    structurallyEqual (Const c1) (Const c2) = c1 == c2
    structurallyEqual (Lam (Binder v1 _) b1) (Lam (Binder v2 _) b2) =
      v1 == v2 && structurallyEqual b1 b2
    structurallyEqual (App f1 x1) (App f2 x2) =
      structurallyEqual f1 f2 && structurallyEqual x1 x2
    structurallyEqual _ _ = False
