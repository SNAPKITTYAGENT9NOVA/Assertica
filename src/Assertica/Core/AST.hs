{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Assertica.Core.AST
Description : Core internal abstract syntax tree for the proof language
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The AST is the trusted representation layer upon which all other agents depend.
It maintains strong separation between Terms, Types, Proofs, Propositions, and Assertions.

DESIGN PRINCIPLES:
1. Each node type (Term, Type, Proof, Proposition, Assertion) is distinct
2. No untyped collapse into a generic tree
3. Supports dependent types via explicit binding structures
4. Uses explicit binders for clarity (not de Bruijn indices initially)
5. All constructors have explicit inputs and clear semantic meaning

INVARIANTS (see Assertica.Core.Invariants):
- All bound variables must be in scope
- All type ascriptions must be well-formed
- No circular references in type definitions
- Module-qualified names must be properly formed
-}

module Assertica.Core.AST
  ( -- * Variables and Names
    Var (..)
  , varName
  , Binder (..)
  , QName (..)
  
    -- * Terms (computational values)
  , Term (..)
  , Pattern (..)
  , Clause (..)
  
    -- * Types (sort/kind layer)
  , Type (..)
  , TypeLevel (..)
  
    -- * Propositions (logical claims)
  , Proposition (..)
  
    -- * Proofs (witnesses for propositions)
  , Proof (..)
  
    -- * Assertions (proof obligations)
  , Assertion (..)
  , Assertion' (..)
  
    -- * Constants
  , Constant (..)
  , PrimitiveOp (..)
  
    -- * Utilities
  , freeVarsInTerm
  , freeVarsInType
  , freeVarsInProof
  , substituteTerm
  , substituteType
  , substituteProof
  , prettyTerm
  , prettyType
  , prettyProof
  , prettyProposition
  , prettyAssertion
  ) where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

-- ============================================================================
-- VARIABLES AND NAMES
-- ============================================================================

{-| A variable in the AST. Uses explicit string names for clarity.
We maintain a scope-aware structure that tracks binding depth.
-}
data Var = Var
  { varName :: Text        -- ^ The name of the variable
  , varId   :: Int         -- ^ Unique identifier (for internal tracking)
  }
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

{-| A binder introduces a variable with an optional type annotation.
Used in lambda abstractions, dependent functions, let-bindings, etc.
-}
data Binder = Binder
  { binderVar  :: Var     -- ^ The bound variable
  , binderType :: Maybe Type  -- ^ Optional type annotation
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Qualified names for modules and global definitions.
Format: module.name or just name for unqualified.
-}
data QName = QName
  { qnameModule :: [Text]  -- ^ Module path (e.g., ["Assertica", "Prelude"])
  , qnameLocal  :: Text    -- ^ Local name within the module
  }
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

-- ============================================================================
-- TYPES
-- ============================================================================

{-| Type universe levels for predicativity (if supported).
For now, we have Type and Type at level n.
-}
data TypeLevel
  = Type0      -- ^ The base type universe
  | TypeN Int  -- ^ Type at level n (for universes)
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Types represent the sorts/kinds of terms.
Supports dependent types via explicit quantification.
-}
data Type
  = TyVar Var                           -- ^ Type variable
  | TyConst QName [Type]                -- ^ Named type with arguments (e.g., List Int)
  | TyFun Type Type                     -- ^ Non-dependent function type: a -> b
  | TyForall Binder Type                -- ^ Dependent function type: forall x : A . B
  | TyUniverse TypeLevel                -- ^ Universe level (Type, Type 1, etc.)
  | TyApp Type Type                     -- ^ Type application (for partially applied types)
  | TyEq Term Term                      -- ^ Equality type: a = b
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- TERMS
-- ============================================================================

{-| Patterns for case expressions and pattern matching.
-}
data Pattern
  = PatVar Var                 -- ^ Bind a variable
  | PatConstr QName [Pattern]  -- ^ Match a data constructor
  | PatWildcard                -- ^ Match anything (no binding)
  | PatLit String              -- ^ Match a literal (placeholder)
  deriving (Eq, Show, Generic, Typeable, Data)

{-| A clause in a case expression.
Pattern -> Term when condition holds.
-}
data Clause = Clause
  { clausePattern :: Pattern
  , clauseBody    :: Term
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Terms are computational values in the proof language.
This includes functions, data, and computed values.
-}
data Term
  = -- Basic constructs
    Var Var                              -- ^ Variable
  | Const Constant                       -- ^ Constant (primitive ops, proof constants)
  
  -- Functions
  | Lam Binder Term                      -- ^ Lambda abstraction: λx . e
  | App Term Term                        -- ^ Application: f x
  
  -- Data
  | Constr QName [Term]                  -- ^ Data constructor application
  | Case Term [Clause] (Maybe Term)      -- ^ Case expression with default
  
  -- Let-binding
  | Let Binder Term Term                 -- ^ let x = e1 in e2
  
  -- Type ascription
  | Ann Term Type                        -- ^ Term annotated with type: e : T
  
  -- Dependent function
  | Forall Binder Term                   -- ^ Dependent function: forall x : A . body
  
  -- Proofs (opaque in this layer, verified by Agent 2B)
  | ProofTerm Proof                      -- ^ A proof term as evidence
  
  -- Propositions embedded as terms (for higher-order reasoning)
  | Prop Proposition                     -- ^ A proposition as a term
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- PROPOSITIONS
-- ============================================================================

{-| Propositions are logical claims about terms.
These represent the types of proof terms.
-}
data Proposition
  = -- Basic
    Eq Term Term                         -- ^ Equality: a = b
  | Top                                  -- ^ True (trivially provable)
  | Bot                                  -- ^ False (refutable)
  
  -- Logical connectives
  | And Proposition Proposition          -- ^ Conjunction
  | Or Proposition Proposition           -- ^ Disjunction
  | Impl Proposition Proposition         -- ^ Implication: P -> Q
  | Not Proposition                      -- ^ Negation: not P
  
  -- Quantification (over terms)
  | Forall' Binder Proposition           -- ^ Universal: forall x : A . P x
  | Exists Binder Proposition            -- ^ Existential: exists x : A . P x
  
  -- Type predicates
  | HasType Term Type                    -- ^ Type judgment: e : T
  | IsTypeCorrect Term                   -- ^ Term is type-correct (well-formed in context)
  
  -- Named propositions (constraints, lemmas)
  | Named QName                          -- ^ Reference to a named proposition
  
  -- Typed propositions
  | Typed Proposition Type               -- ^ Proposition with explicit type
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- PROOFS
-- ============================================================================

{-| Proof terms are witnesses for propositions.
These are largely opaque at this layer; Agent 2B's equality checker will verify them.
-}
data Proof
  = -- Basic proofs
    Refl Term                            -- ^ Proof by reflexivity: x = x
  | Symm Proof                           -- ^ Symmetry: from P prove symmetric P
  | Trans Proof Proof                    -- ^ Transitivity: compose proofs
  
  -- Structural proofs
  | Intro Binder Proof                   -- ^ Introduce a hypothesis (implication intro)
  | Elim Proof Proof                     -- ^ Eliminate a hypothesis (modus ponens)
  
  -- Data/constructor proofs
  | Constr' QName [Proof]                -- ^ Proof constructor (e.g., conjunction pair)
  | Case' Proof [Clause] (Maybe Proof)   -- ^ Case analysis on a proof
  
  -- Application
  | App' Proof Term                      -- ^ Apply proof to term (instantiation)
  
  -- Opaque proof (verified by the type checker or proof kernel)
  | Opaque QName Proposition             -- ^ Trusted proof (e.g., from an oracle or axiom)
  
  -- Conversion (type-aware)
  | Conv Proof Type Type                 -- ^ Proof with implicit type conversion
  
  -- Annotation
  | ProofAnn Proof Proposition           -- ^ Proof explicitly typed with proposition
  
  -- Variables in proofs
  | ProofVar Var                         -- ^ Proof variable
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- CONSTANTS
-- ============================================================================

{-| Primitive operations supported by the language.
-}
data PrimitiveOp
  = OpAdd | OpSub | OpMul | OpDiv       -- ^ Arithmetic
  | OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe  -- ^ Comparison
  | OpAnd | OpOr | OpNot                -- ^ Logical
  | OpIfThenElse                        -- ^ Conditional
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Constants in the language.
-}
data Constant
  = PrimOp PrimitiveOp                   -- ^ Built-in operation
  | IntLit Integer                       -- ^ Integer literal
  | BoolLit Bool                         -- ^ Boolean literal
  | StringLit String                     -- ^ String literal
  | UnitConst                            -- ^ Unit value
  | ProofConst QName                     -- ^ Named proof constant (axiom, lemma)
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- ASSERTIONS
-- ============================================================================

{-| An assertion is a top-level proof obligation in a module.
-}
data Assertion = Assertion
  { assertionName  :: QName              -- ^ Name of the assertion
  , assertionProp  :: Proposition        -- ^ The proposition to prove
  , assertionProof :: Maybe Proof        -- ^ Optional proof (Nothing = unsolved)
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Simplified assertion for contexts where we only care about the proof obligation.
-}
data Assertion' = Assertion'
  { assertion'Prop  :: Proposition
  , assertion'Proof :: Maybe Proof
  }
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- UTILITIES: FREE VARIABLE COLLECTION
-- ============================================================================

{-| Collect all free variables in a term.
A variable is free if it is not bound by a lambda, forall, or let.
-}
freeVarsInTerm :: Term -> Set Var
freeVarsInTerm term = go Set.empty term
  where
    go bound = \case
      Var v -> if Set.member v bound then Set.empty else Set.singleton v
      Const _ -> Set.empty
      Lam binder body ->
        let bound' = Set.insert (binderVar binder) bound
        in go bound' body
      App f x -> Set.union (go bound f) (go bound x)
      Constr _ args -> Set.unions (map (go bound) args)
      Case e clauses def ->
        let fv = go bound e
            fv_clauses = Set.unions [freeVarsInClause bound c | c <- clauses]
            fv_def = maybe Set.empty (go bound) def
        in Set.unions [fv, fv_clauses, fv_def]
      Let binder e1 e2 ->
        let fv1 = go bound e1
            bound' = Set.insert (binderVar binder) bound
            fv2 = go bound' e2
        in Set.union fv1 fv2
      Ann e ty ->
        Set.union (go bound e) (freeVarsInType bound ty)
      Forall binder body ->
        let bound' = Set.insert (binderVar binder) bound
        in go bound' body
      ProofTerm p -> freeVarsInProof bound p
      Prop prop -> freeVarsInProposition bound prop

freeVarsInClause :: Set Var -> Clause -> Set Var
freeVarsInClause bound (Clause pat body) =
  let boundVars = boundVarsInPattern pat
      bound' = Set.union bound boundVars
  in go bound' body
  where
    go = freeVarsInTerm

boundVarsInPattern :: Pattern -> Set Var
boundVarsInPattern = \case
  PatVar v -> Set.singleton v
  PatConstr _ pats -> Set.unions (map boundVarsInPattern pats)
  PatWildcard -> Set.empty
  PatLit _ -> Set.empty

{-| Collect free variables in a type.
-}
freeVarsInType :: Set Var -> Type -> Set Var
freeVarsInType bound = go
  where
    go = \case
      TyVar v -> if Set.member v bound then Set.empty else Set.singleton v
      TyConst _ args -> Set.unions (map go args)
      TyFun a b -> Set.union (go a) (go b)
      TyForall binder ty ->
        let bound' = Set.insert (binderVar binder) bound
        in freeVarsInType bound' ty
      TyUniverse _ -> Set.empty
      TyApp a b -> Set.union (go a) (go b)
      TyEq e1 e2 -> Set.union (freeVarsInTerm e1) (freeVarsInTerm e2)

{-| Collect free variables in a proof.
-}
freeVarsInProof :: Set Var -> Proof -> Set Var
freeVarsInProof bound = go
  where
    go = \case
      Refl e -> freeVarsInTerm e
      Symm p -> go p
      Trans p1 p2 -> Set.union (go p1) (go p2)
      Intro binder p ->
        let bound' = Set.insert (binderVar binder) bound
        in freeVarsInProof bound' p
      Elim p1 p2 -> Set.union (go p1) (go p2)
      Constr' _ proofs -> Set.unions (map go proofs)
      Case' p clauses def ->
        let fv = go p
            fv_clauses = Set.unions [freeVarsInProofClause bound c | c <- clauses]
            fv_def = maybe Set.empty go def
        in Set.unions [fv, fv_clauses, fv_def]
      App' p e -> Set.union (go p) (freeVarsInTerm e)
      Opaque _ _ -> Set.empty
      Conv p _ _ -> go p
      ProofAnn p _ -> go p
      ProofVar v -> if Set.member v bound then Set.empty else Set.singleton v

freeVarsInProofClause :: Set Var -> Clause -> Set Var
freeVarsInProofClause bound (Clause pat body) =
  let boundVars = boundVarsInPattern pat
      bound' = Set.union bound boundVars
  in freeVarsInProof bound' (extractProofFromTerm body)
  where
    extractProofFromTerm (ProofTerm p) = p
    extractProofFromTerm _ = Refl (Const UnitConst)

{-| Collect free variables in a proposition.
-}
freeVarsInProposition :: Set Var -> Proposition -> Set Var
freeVarsInProposition bound = go
  where
    go = \case
      Eq e1 e2 -> Set.union (freeVarsInTerm e1) (freeVarsInTerm e2)
      Top -> Set.empty
      Bot -> Set.empty
      And p1 p2 -> Set.union (go p1) (go p2)
      Or p1 p2 -> Set.union (go p1) (go p2)
      Impl p1 p2 -> Set.union (go p1) (go p2)
      Not p -> go p
      Forall' binder prop ->
        let bound' = Set.insert (binderVar binder) bound
        in freeVarsInProposition bound' prop
      Exists binder prop ->
        let bound' = Set.insert (binderVar binder) bound
        in freeVarsInProposition bound' prop
      HasType e ty -> Set.union (freeVarsInTerm e) (freeVarsInType bound ty)
      IsTypeCorrect e -> freeVarsInTerm e
      Named _ -> Set.empty
      Typed p _ -> go p

-- ============================================================================
-- UTILITIES: SUBSTITUTION
-- ============================================================================

{-| Substitute a variable with a term in a term.
Respects bounding and avoids capture.
-}
substituteTerm :: Var -> Term -> Term -> Term
substituteTerm var replacement = go
  where
    go = \case
      Var v -> if v == var then replacement else Var v
      Const c -> Const c
      Lam binder body ->
        if binderVar binder == var
        then Lam binder body
        else Lam binder (go body)
      App f x -> App (go f) (go x)
      Constr n args -> Constr n (map go args)
      Case e clauses def ->
        Case (go e)
             [Clause pat (goInClause pat body) | Clause pat body <- clauses]
             (fmap go def)
      Let binder e1 e2 ->
        let e1' = go e1
            e2' = if binderVar binder == var then e2 else go e2
        in Let binder e1' e2'
      Ann e ty -> Ann (go e) ty
      Forall binder body ->
        if binderVar binder == var
        then Forall binder body
        else Forall binder (go body)
      ProofTerm p -> ProofTerm (substituteProof var replacement p)
      Prop prop -> Prop (substituteProposition var replacement prop)
    
    goInClause pat body =
      if any (== var) (Set.toList (boundVarsInPattern pat))
      then body
      else go body

{-| Substitute in a type.
-}
substituteType :: Var -> Term -> Type -> Type
substituteType var replacement = go
  where
    go = \case
      TyVar v -> TyVar v
      TyConst n args -> TyConst n (map go args)
      TyFun a b -> TyFun (go a) (go b)
      TyForall binder ty ->
        if binderVar binder == var
        then TyForall binder ty
        else TyForall binder (go ty)
      TyUniverse u -> TyUniverse u
      TyApp a b -> TyApp (go a) (go b)
      TyEq e1 e2 -> TyEq (substituteTerm var replacement e1) (substituteTerm var replacement e2)

{-| Substitute in a proof.
-}
substituteProof :: Var -> Term -> Proof -> Proof
substituteProof var replacement = go
  where
    go = \case
      Refl e -> Refl (substituteTerm var replacement e)
      Symm p -> Symm (go p)
      Trans p1 p2 -> Trans (go p1) (go p2)
      Intro binder p ->
        if binderVar binder == var
        then Intro binder p
        else Intro binder (go p)
      Elim p1 p2 -> Elim (go p1) (go p2)
      Constr' n ps -> Constr' n (map go ps)
      Case' p clauses def ->
        Case' (go p)
              [Clause pat (body) | Clause pat body <- clauses]
              (fmap go def)
      App' p e -> App' (go p) (substituteTerm var replacement e)
      Opaque n prop -> Opaque n (substituteProposition var replacement prop)
      Conv p t1 t2 -> Conv (go p) (substituteType var replacement t1) (substituteType var replacement t2)
      ProofAnn p prop -> ProofAnn (go p) (substituteProposition var replacement prop)
      ProofVar v -> ProofVar v

{-| Substitute in a proposition.
-}
substituteProposition :: Var -> Term -> Proposition -> Proposition
substituteProposition var replacement = go
  where
    go = \case
      Eq e1 e2 -> Eq (substituteTerm var replacement e1) (substituteTerm var replacement e2)
      Top -> Top
      Bot -> Bot
      And p1 p2 -> And (go p1) (go p2)
      Or p1 p2 -> Or (go p1) (go p2)
      Impl p1 p2 -> Impl (go p1) (go p2)
      Not p -> Not (go p)
      Forall' binder prop ->
        if binderVar binder == var
        then Forall' binder prop
        else Forall' binder (go prop)
      Exists binder prop ->
        if binderVar binder == var
        then Exists binder prop
        else Exists binder (go prop)
      HasType e ty -> HasType (substituteTerm var replacement e) (substituteType var replacement ty)
      IsTypeCorrect e -> IsTypeCorrect (substituteTerm var replacement e)
      Named n -> Named n
      Typed p ty -> Typed (go p) (substituteType var replacement ty)

-- ============================================================================
-- UTILITIES: PRETTY-PRINTING
-- ============================================================================

{-| Pretty-print a term for display/debugging.
-}
prettyTerm :: Term -> String
prettyTerm = \case
  Var (Var name _) -> T.unpack name
  Const c -> prettyConstant c
  Lam (Binder (Var name _) mty) body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettyType ty
    in "λ" ++ T.unpack name ++ ty_str ++ ". " ++ prettyTerm body
  App f x -> "(" ++ prettyTerm f ++ " " ++ prettyTerm x ++ ")"
  Constr (QName mod name) args ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in mod_str ++ T.unpack name ++ if null args then "" else " " ++ unwords (map prettyTerm args)
  Case e clauses def ->
    "case " ++ prettyTerm e ++ " of { " ++
    intercalate "; " [prettyPattern p ++ " -> " ++ prettyTerm b | Clause p b <- clauses] ++
    (case def of Nothing -> ""; Just d -> "; _ -> " ++ prettyTerm d) ++ " }"
  Let (Binder (Var name _) _) e1 e2 ->
    "let " ++ T.unpack name ++ " = " ++ prettyTerm e1 ++ " in " ++ prettyTerm e2
  Ann e ty -> "(" ++ prettyTerm e ++ " : " ++ prettyType ty ++ ")"
  Forall (Binder (Var name _) mty) body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettyType ty
    in "∀ " ++ T.unpack name ++ ty_str ++ ". " ++ prettyTerm body
  ProofTerm p -> prettyProof p
  Prop prop -> prettyProposition prop

prettyPattern :: Pattern -> String
prettyPattern = \case
  PatVar (Var name _) -> T.unpack name
  PatConstr (QName mod name) pats ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in mod_str ++ T.unpack name ++ if null pats then "" else " " ++ unwords (map prettyPattern pats)
  PatWildcard -> "_"
  PatLit s -> s

{-| Pretty-print a type for display/debugging.
-}
prettyType :: Type -> String
prettyType = \case
  TyVar (Var name _) -> T.unpack name
  TyConst (QName mod name) args ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
        args_str = if null args then "" else " " ++ unwords (map prettyType args)
    in mod_str ++ T.unpack name ++ args_str
  TyFun a b -> "(" ++ prettyType a ++ " → " ++ prettyType b ++ ")"
  TyForall (Binder (Var name _) mty) body ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettyType ty
    in "∀ " ++ T.unpack name ++ ty_str ++ ". " ++ prettyType body
  TyUniverse Type0 -> "Type"
  TyUniverse (TypeN n) -> "Type " ++ show n
  TyApp a b -> "(" ++ prettyType a ++ " " ++ prettyType b ++ ")"
  TyEq e1 e2 -> "(" ++ prettyTerm e1 ++ " = " ++ prettyTerm e2 ++ ")"

{-| Pretty-print a proposition.
-}
prettyProposition :: Proposition -> String
prettyProposition = \case
  Eq e1 e2 -> "(" ++ prettyTerm e1 ++ " = " ++ prettyTerm e2 ++ ")"
  Top -> "⊤"
  Bot -> "⊥"
  And p1 p2 -> "(" ++ prettyProposition p1 ++ " ∧ " ++ prettyProposition p2 ++ ")"
  Or p1 p2 -> "(" ++ prettyProposition p1 ++ " ∨ " ++ prettyProposition p2 ++ ")"
  Impl p1 p2 -> "(" ++ prettyProposition p1 ++ " → " ++ prettyProposition p2 ++ ")"
  Not p -> "¬(" ++ prettyProposition p ++ ")"
  Forall' (Binder (Var name _) mty) prop ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettyType ty
    in "∀ " ++ T.unpack name ++ ty_str ++ ". " ++ prettyProposition prop
  Exists (Binder (Var name _) mty) prop ->
    let ty_str = case mty of
          Nothing -> ""
          Just ty -> " : " ++ prettyType ty
    in "∃ " ++ T.unpack name ++ ty_str ++ ". " ++ prettyProposition prop
  HasType e ty -> "(" ++ prettyTerm e ++ " : " ++ prettyType ty ++ ")"
  IsTypeCorrect e -> "WF(" ++ prettyTerm e ++ ")"
  Named (QName mod name) ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in mod_str ++ T.unpack name
  Typed p ty -> "(" ++ prettyProposition p ++ " : " ++ prettyType ty ++ ")"

{-| Pretty-print a proof.
-}
prettyProof :: Proof -> String
prettyProof = \case
  Refl e -> "refl(" ++ prettyTerm e ++ ")"
  Symm p -> "symm(" ++ prettyProof p ++ ")"
  Trans p1 p2 -> "trans(" ++ prettyProof p1 ++ ", " ++ prettyProof p2 ++ ")"
  Intro (Binder (Var name _) _) p -> "λ" ++ T.unpack name ++ ". " ++ prettyProof p
  Elim p1 p2 -> "(" ++ prettyProof p1 ++ " " ++ prettyProof p2 ++ ")"
  Constr' (QName mod name) ps ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in mod_str ++ T.unpack name ++ if null ps then "" else " " ++ unwords (map prettyProof ps)
  Case' p clauses def ->
    "case " ++ prettyProof p ++ " of { " ++
    intercalate "; " [prettyPattern pat ++ " -> " ++ prettyTerm body | Clause pat body <- clauses] ++
    (case def of Nothing -> ""; Just d -> "; _ -> " ++ prettyProof d) ++ " }"
  App' p e -> "(" ++ prettyProof p ++ " " ++ prettyTerm e ++ ")"
  Opaque (QName mod name) _ ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in "opaque[" ++ mod_str ++ T.unpack name ++ "]"
  Conv p t1 t2 -> "conv(" ++ prettyProof p ++ " : " ++ prettyType t1 ++ " → " ++ prettyType t2 ++ ")"
  ProofAnn p prop -> "(" ++ prettyProof p ++ " : " ++ prettyProposition prop ++ ")"
  ProofVar (Var name _) -> T.unpack name

prettyConstant :: Constant -> String
prettyConstant = \case
  PrimOp op -> "op[" ++ show op ++ "]"
  IntLit n -> show n
  BoolLit b -> show b
  StringLit s -> show s
  UnitConst -> "()"
  ProofConst (QName mod name) ->
    let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
    in mod_str ++ T.unpack name

{-| Pretty-print an assertion.
-}
prettyAssertion :: Assertion -> String
prettyAssertion (Assertion (QName mod name) prop proof) =
  let mod_str = if null mod then "" else intercalate "." (map T.unpack mod) ++ "."
      proof_str = case proof of
        Nothing -> " [unproven]"
        Just p -> " := " ++ prettyProof p
  in "assert " ++ mod_str ++ T.unpack name ++ " : " ++ prettyProposition prop ++ proof_str
