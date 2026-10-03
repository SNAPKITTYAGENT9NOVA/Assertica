{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}
{-# LANGUAGE DeriveGeneric #-}

module Assertica.Core.AST
  ( Term(..)
  , Var(..)
  , Definition
  , DefinitionEnv
  , emptyEnv
  , lookupDef
  , extendEnv
  , freeVars
  , boundVars
  , substitute
  , alphaRename
  -- * Propositions (Agent 2A)
  , Proposition(..)
  , SourceLoc(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Variable name
newtype Var = Var String
  deriving (Eq, Ord, Show, Generic, Typeable)

-- | A term in the language
data Term
  = TVar Var                      -- ^ Variable: x
  | TAbs Var Term                 -- ^ Abstraction: λx. body
  | TApp Term Term                -- ^ Application: f x
  | TConst String                 -- ^ Constant: 0, 1, true, false, etc.
  | TLet Var Term Term            -- ^ Let-binding: let x = value in body
  | TPair Term Term               -- ^ Pair: (a, b)
  | TFst Term                     -- ^ First projection
  | TSnd Term                     -- ^ Second projection
  deriving (Eq, Show, Generic, Typeable)

-- | A definition: name and body
type Definition = (Var, Term)

-- | Environment of definitions
type DefinitionEnv = Map.Map Var Term

-- | Empty definition environment
emptyEnv :: DefinitionEnv
emptyEnv = Map.empty

-- | Look up a definition
lookupDef :: Var -> DefinitionEnv -> Maybe Term
lookupDef = Map.lookup

-- | Extend environment with a new definition
extendEnv :: Var -> Term -> DefinitionEnv -> DefinitionEnv
extendEnv = Map.insert

-- | Compute free variables in a term
freeVars :: Term -> Set.Set Var
freeVars (TVar v) = Set.singleton v
freeVars (TAbs v body) = Set.delete v (freeVars body)
freeVars (TApp f x) = Set.union (freeVars f) (freeVars x)
freeVars (TConst _) = Set.empty
freeVars (TLet v val body) = Set.union (freeVars val) (Set.delete v (freeVars body))
freeVars (TPair a b) = Set.union (freeVars a) (freeVars b)
freeVars (TFst t) = freeVars t
freeVars (TSnd t) = freeVars t

-- | Compute bound variables in a term
boundVars :: Term -> Set.Set Var
boundVars (TVar _) = Set.empty
boundVars (TAbs v body) = Set.insert v (boundVars body)
boundVars (TApp f x) = Set.union (boundVars f) (boundVars x)
boundVars (TConst _) = Set.empty
boundVars (TLet v val body) = Set.insert v (Set.union (boundVars val) (boundVars body))
boundVars (TPair a b) = Set.union (boundVars a) (boundVars b)
boundVars (TFst t) = boundVars t
boundVars (TSnd t) = boundVars t

-- | Substitute a variable with a term
-- substitute x replacement term: replaces all free occurrences of x in term with replacement
substitute :: Var -> Term -> Term -> Term
substitute x replacement term = go term
  where
    go (TVar v) = if v == x then replacement else TVar v
    go (TAbs v body)
      | v == x = TAbs v body  -- Bound variable shadows the substitution
      | v `Set.member` freeVars replacement =
          -- Need to rename v to avoid capture
          let freshVar = freshenVar v term replacement
          in TAbs freshVar (substitute x replacement (alphaRename v freshVar body))
      | otherwise = TAbs v (go body)
    go (TApp f arg) = TApp (go f) (go arg)
    go (TConst c) = TConst c
    go (TLet v val body)
      | v == x = TLet v (go val) body  -- Bound variable shadows the substitution
      | v `Set.member` freeVars replacement =
          let freshVar = freshenVar v term replacement
          in TLet freshVar (go val) (substitute x replacement (alphaRename v freshVar body))
      | otherwise = TLet v (go val) (go body)
    go (TPair a b) = TPair (go a) (go b)
    go (TFst t) = TFst (go t)
    go (TSnd t) = TSnd (go t)

-- | Alpha-rename: replace all occurrences of one variable with another
alphaRename :: Var -> Var -> Term -> Term
alphaRename oldVar newVar = go
  where
    go (TVar v) = if v == oldVar then TVar newVar else TVar v
    go (TAbs v body)
      | v == oldVar = TAbs newVar (go body)
      | otherwise = TAbs v (go body)
    go (TApp f x) = TApp (go f) (go x)
    go (TConst c) = TConst c
    go (TLet v val body)
      | v == oldVar = TLet newVar (go val) (go body)
      | otherwise = TLet v (go val) (go body)
    go (TPair a b) = TPair (go a) (go b)
    go (TFst t) = TFst (go t)
    go (TSnd t) = TSnd (go t)

-- | Generate a fresh variable that doesn't appear in the term or the replacement
freshenVar :: Var -> Term -> Term -> Var
freshenVar (Var v) term replacement =
  let occupied = Set.union (freeVars term) (freeVars replacement)
      candidates = map (\n -> Var (v ++ "_" ++ show n)) [1..]
  in head [c | c <- candidates, c `Set.notMember` occupied]

-- ============================================================================
-- Proposition AST (Agent 2A: Explicit Assertion System)
-- ============================================================================

-- | Source location information for error reporting
data SourceLoc = SourceLoc
  { sourcePath :: String
  , lineNumber :: Int
  , columnNumber :: Int
  }
  deriving (Eq, Show, Generic, Typeable)

-- | A proposition represents a mathematical obligation
-- These are NOT proofs, but proof goals that must be discharged by Agent 2B
data Proposition
  = -- | Equality proposition: a ≡ b
    --   Two terms are structurally equal
    EqualityProp Term Term

  | -- | Universal quantification: ∀ x. P x
    --   A property holds for all values of a variable
    UniversalQuantProp Var Proposition

  | -- | Implication: P → Q
    --   If P holds, then Q must hold
    ImplicationProp Proposition Proposition

  | -- | Conjunction: P ∧ Q
    --   Both P and Q must hold
    ConjunctionProp Proposition Proposition

  | -- | Type membership / typing proposition: x : T
    --   A term has a particular type
    TypingProp Term String  -- String is the type name (we can extend this later)

  | -- | Predicate application: P(t1, t2, ...)
    --   A named predicate applied to terms
    PredicateApp String [Term]

  | -- | Negation: ¬P
    --   P does not hold
    NegationProp Proposition

  | -- | Disjunction: P ∨ Q
    --   At least one of P or Q holds
    DisjunctionProp Proposition Proposition

  deriving (Eq, Show, Generic, Typeable)
