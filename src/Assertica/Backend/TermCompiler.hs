{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Backend.TermCompiler
Description : Compile Assertica terms to Haskell expressions
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Translates computational terms from the Core AST to Haskell expressions:

MAPPINGS:

- Var v                     → v (variable reference)
- Const c                   → haskellLiteral c (constant)
- Lam (x : T) . e           → \x -> <compiled e> (lambda)
- App f x                   → (<compiled f>) (<compiled x>) (application)
- Constr C [e1, ..., en]    → C e1 ... en (constructor application)
- Case e [p1->b1, ...] def  → case e of { p1 -> b1; ... } (case expression)
- Let x = e1 in e2          → let x = <compiled e1> in <compiled e2>
- Ann e T                   → (<compiled e> :: <compiled T>) (type ascription)
- Forall (x : T) . body     → \(x :: <compiled T>) -> <compiled body> (dependent fn)
- ProofTerm p               → <compiled p> (proof evidence)
- Prop prop                 → proof-irrelevant <compiled p> (propositions as terms)

PRINCIPLES:

1. **Deterministic**: Always produces same output for same input
2. **Preserve Beta-Equivalence**: Compilation respects computational semantics
3. **Scope Preservation**: Free/bound variable distinction maintained
4. **No Code Simplification**: Generate direct translation (optimizer's job)
-}

module Assertica.Backend.TermCompiler
  ( -- * Main compilation function
    compileTerm
  , compilePattern
  , compileClause
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Assertica.Core.AST
import Assertica.Backend.CodeGen (CodeGenConfig)

-- ============================================================================
-- MAIN COMPILATION FUNCTION
-- ============================================================================

{-| Compile a term to Haskell source code.

This is the primary entry point for term compilation.
Returns either a Haskell expression string or an error.
-}
compileTerm :: CodeGenConfig -> Term -> Either String Text
compileTerm config term = compileTermWith config Set.empty term

{-| Compile a term with a given bound variable context.

The bound context tracks variables that are bound by enclosing lambdas,
let-bindings, or other binders, to ensure proper scoping.
-}
compileTermWith :: CodeGenConfig -> Set.Set Var -> Term -> Either String Text
compileTermWith config bound = \case
  Var (Var name _) ->
    Right $ name

  Const c -> Right $ compileConstant c

  Lam (Binder (Var name _) _mty) body ->
    let bound' = Set.insert (Var name 0) bound  -- Simplified: just track name
    in do
      compiledBody <- compileTermWith config bound' body
      Right $ "(\\(" <> name <> ") -> " <> compiledBody <> ")"

  App f x -> do
    cf <- compileTermWith config bound f
    cx <- compileTermWith config bound x
    Right $ "(" <> cf <> " " <> cx <> ")"

  Constr (QName mod name) args -> do
    compiledArgs <- mapM (compileTermWith config bound) args
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    if null compiledArgs
      then Right qname
      else Right $ "(" <> qname <> " " <> T.intercalate " " compiledArgs <> ")"

  Case scrutinee clauses defaultClause -> do
    cs <- compileTermWith config bound scrutinee
    compiledClauses <- mapM (compileClause config bound) clauses

    let caseClauses = T.intercalate "; " compiledClauses
    let defaultPart = case defaultClause of
          Nothing -> ""
          Just d -> do
            cd <- compileTermWith config bound d
            "; _ -> " <> cd

    Right $ "(case " <> cs <> " of { " <> caseClauses <> defaultPart <> " })"

  Let (Binder (Var name _) _) bindExpr bodyExpr -> do
    cb <- compileTermWith config bound bindExpr
    let bound' = Set.insert (Var name 0) bound
    ce <- compileTermWith config bound' bodyExpr
    Right $ "(let " <> name <> " = " <> cb <> " in " <> ce <> ")"

  Ann term _ty -> do
    -- For now, ignore the type annotation in code gen
    -- In a full implementation, could preserve as Haskell type ascription
    compileTermWith config bound term

  Forall (Binder (Var name _) _) body -> do
    -- Dependent function: treat as lambda abstraction
    let bound' = Set.insert (Var name 0) bound
    cb <- compileTermWith config bound' body
    Right $ "(\\(" <> name <> ") -> " <> cb <> ")"

  ProofTerm p -> do
    -- Proofs compile to evidence terms
    compileProofTerm config bound p

  Prop prop -> do
    -- Propositions as terms become proof-irrelevant ()
    -- The proposition itself is erased (proof irrelevance)
    Right "()"

-- ============================================================================
-- PATTERN COMPILATION
-- ============================================================================

{-| Compile a pattern to Haskell pattern syntax -}
compilePattern :: Pattern -> Either String Text
compilePattern = \case
  PatVar (Var name _) -> Right name

  PatConstr (QName mod name) pats -> do
    compiledPats <- mapM compilePattern pats
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    if null compiledPats
      then Right qname
      else Right $ "(" <> qname <> " " <> T.intercalate " " compiledPats <> ")"

  PatWildcard -> Right "_"

  PatLit lit -> Right $ T.pack lit

{-| Compile a single case clause -}
compileClause :: CodeGenConfig -> Set.Set Var -> Clause -> Either String Text
compileClause config bound (Clause pat body) = do
  cp <- compilePattern pat
  cb <- compileTermWith config bound body
  Right $ cp <> " -> " <> cb

-- ============================================================================
-- CONSTANT COMPILATION
-- ============================================================================

{-| Compile a constant value -}
compileConstant :: Constant -> Text
compileConstant = \case
  PrimOp op -> compilePrimOp op
  IntLit n -> T.pack $ show n
  BoolLit b -> T.pack $ show b
  StringLit s -> T.pack $ show s
  UnitConst -> "()"
  ProofConst (QName mod name) ->
    if null mod
      then name
      else T.intercalate "." mod <> "." <> name

{-| Compile a primitive operation to Haskell -}
compilePrimOp :: PrimitiveOp -> Text
compilePrimOp = \case
  OpAdd -> "(+)"
  OpSub -> "(-)"
  OpMul -> "(*)"
  OpDiv -> "div"
  OpEq -> "(==)"
  OpNeq -> "(/=)"
  OpLt -> "(<)"
  OpLe -> "(<=)"
  OpGt -> "(>)"
  OpGe -> "(>=)"
  OpAnd -> "(&&)"
  OpOr -> "(||)"
  OpNot -> "not"
  OpIfThenElse -> "if"  -- Special handling in caller

-- ============================================================================
-- PROOF TERM COMPILATION (provisional)
-- ============================================================================

{-| Compile a proof term to Haskell evidence.

This is a simplified version that handles basic proof terms.
Full proof term handling is delegated to ProofCompiler.
-}
compileProofTerm :: CodeGenConfig -> Set.Set Var -> Proof -> Either String Text
compileProofTerm _config _bound = \case
  Refl e -> Right "Refl"
  Symm _ -> Right "Symm"
  Trans _ _ -> Right "Trans"
  ProofVar (Var name _) -> Right name
  _ -> Right "()"  -- Default: unit for unhandled proofs
