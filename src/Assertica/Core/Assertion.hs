{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Core.Assertion
Description : Explicit assertion system for mathematical obligations
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The assertion system represents mathematical obligations as first-class program entities.
An assertion is an explicit proof goal that must be discharged by the proof-term checker.

Key design principles:
  1. Explicit representation: Every assertion is a data structure, not a string or comment.
  2. Fail-closed: An unknown or malformed assertion is UNPROVEN, not silently accepted.
  3. Proof obligation boundary: The assertion clearly identifies what must be proven.
  4. Composable propositions: Build complex propositions from simpler ones.
  5. No hidden theorem proving: Assertions state obligations; the proof checker discharges them.

Integration:
  - Input: Elaborator (Agent 3A) produces surface assertions
  - Processing: This module represents obligations
  - Output: Proof-term checker (Agent 2B) discharges proof obligations
-}

module Assertica.Core.Assertion
  ( -- * Core types
    Assertion(..)
  , ProofObligation(..)
  , ObligationState(..)
  , ObligationStore

  -- * Assertion construction and querying
  , mkAssertion
  , assertionName
  , assertionProp
  , assertionLoc
  , getObligations
  , lookupObligation
  , countByState

  -- * Proposition utilities
  , propFreeVariables
  , propBoundVariables
  , propSubstitute
  , propAlphaRename
  , prettyPrintProposition

  -- * Obligation tracking
  , emptyStore
  , insertObligation
  , updateObligation
  , markProven
  , markUnproven
  , markRejected
  , allProven
  , unprovenObligations

  -- * Elaboration interface
  , SurfaceAssertion(..)
  , elaborateAssertion

  -- * Re-exports from AST
  , Proposition(..)
  , Term(..)
  , Var(..)
  , SourceLoc(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.List (intercalate)

import Assertica.Core.AST
  ( Proposition(..)
  , Term(..)
  , Var(..)
  , SourceLoc(..)
  , freeVars
  , substitute
  , alphaRename
  )

-- ============================================================================
-- Core Assertion Type
-- ============================================================================

-- | An assertion is a named proof obligation with a source location
data Assertion = Assertion
  { assertionName :: String
    -- ^ Unique name for the assertion
  , assertionProp :: Proposition
    -- ^ The proposition to be proven
  , assertionLoc :: SourceLoc
    -- ^ Source location for error reporting
  }
  deriving (Eq, Show, Generic, Typeable)

-- | Proof obligation state tracking
data ObligationState
  = Asserted    -- ^ The obligation has been stated but not yet proven
  | Proven      -- ^ A valid proof has been accepted
  | Unproven    -- ^ The proof attempt was invalid or no proof provided
  | Rejected    -- ^ The obligation was explicitly rejected
  deriving (Eq, Show, Ord, Generic, Typeable)

-- | A proof obligation with its current state
data ProofObligation = ProofObligation
  { obligationAssertion :: Assertion
  , obligationState :: ObligationState
  , obligationProofTerm :: Maybe String
    -- ^ Optional reference to the proof term that discharges this obligation
  }
  deriving (Eq, Show, Generic, Typeable)

-- | Storage for proof obligations
-- Maps assertion names to their obligations
type ObligationStore = Map.Map String ProofObligation

-- ============================================================================
-- Assertion Construction
-- ============================================================================

-- | Create an assertion with default Asserted state
mkAssertion :: String -> Proposition -> SourceLoc -> Assertion
mkAssertion name prop loc = Assertion name prop loc

-- ============================================================================
-- Obligation Store Operations
-- ============================================================================

-- | Create an empty obligation store
emptyStore :: ObligationStore
emptyStore = Map.empty

-- | Insert a new obligation into the store
insertObligation :: Assertion -> ObligationStore -> ObligationStore
insertObligation assertion store =
  let obligation = ProofObligation assertion Asserted Nothing
  in Map.insert (assertionName assertion) obligation store

-- | Look up an obligation by assertion name
lookupObligation :: String -> ObligationStore -> Maybe ProofObligation
lookupObligation = Map.lookup

-- | Get all obligations from the store
getObligations :: ObligationStore -> [ProofObligation]
getObligations = Map.elems

-- | Update an obligation's state
updateObligation :: String -> ObligationState -> Maybe String -> ObligationStore -> Either String ObligationStore
updateObligation name state proofTerm store =
  case Map.lookup name store of
    Nothing -> Left $ "Obligation not found: " ++ name
    Just obligation ->
      let updated = obligation { obligationState = state, obligationProofTerm = proofTerm }
      in Right $ Map.insert name updated store

-- | Mark an obligation as proven
markProven :: String -> Maybe String -> ObligationStore -> Either String ObligationStore
markProven name proofTerm = updateObligation name Proven proofTerm

-- | Mark an obligation as unproven
markUnproven :: String -> ObligationStore -> Either String ObligationStore
markUnproven name store = updateObligation name Unproven Nothing store

-- | Mark an obligation as rejected
markRejected :: String -> ObligationStore -> Either String ObligationStore
markRejected name store = updateObligation name Rejected Nothing store

-- | Check if all obligations are proven
allProven :: ObligationStore -> Bool
allProven store = all (\ob -> obligationState ob == Proven) (getObligations store)

-- | Get all unproven obligations
unprovenObligations :: ObligationStore -> [ProofObligation]
unprovenObligations store =
  filter (\ob -> obligationState ob /= Proven) (getObligations store)

-- | Count obligations by state
countByState :: ObligationState -> ObligationStore -> Int
countByState state store =
  length $ filter (\ob -> obligationState ob == state) (getObligations store)

-- ============================================================================
-- Proposition Utilities
-- ============================================================================

-- | Free variables in a proposition
propFreeVariables :: Proposition -> Set.Set Var
propFreeVariables (EqualityProp t1 t2) = Set.union (freeVars t1) (freeVars t2)
propFreeVariables (UniversalQuantProp v prop) =
  Set.delete v (propFreeVariables prop)
propFreeVariables (ImplicationProp p q) =
  Set.union (propFreeVariables p) (propFreeVariables q)
propFreeVariables (ConjunctionProp p q) =
  Set.union (propFreeVariables p) (propFreeVariables q)
propFreeVariables (TypingProp t _) = freeVars t
propFreeVariables (PredicateApp _ terms) =
  Set.unions $ map freeVars terms
propFreeVariables (NegationProp p) = propFreeVariables p
propFreeVariables (DisjunctionProp p q) =
  Set.union (propFreeVariables p) (propFreeVariables q)

-- | Bound variables in a proposition
propBoundVariables :: Proposition -> Set.Set Var
propBoundVariables (EqualityProp t1 t2) = Set.empty  -- Terms have their own binding
propBoundVariables (UniversalQuantProp v prop) =
  Set.insert v (propBoundVariables prop)
propBoundVariables (ImplicationProp p q) =
  Set.union (propBoundVariables p) (propBoundVariables q)
propBoundVariables (ConjunctionProp p q) =
  Set.union (propBoundVariables p) (propBoundVariables q)
propBoundVariables (TypingProp _ _) = Set.empty
propBoundVariables (PredicateApp _ _) = Set.empty
propBoundVariables (NegationProp p) = propBoundVariables p
propBoundVariables (DisjunctionProp p q) =
  Set.union (propBoundVariables p) (propBoundVariables q)

-- | Substitute a variable in a proposition
-- Must handle binding correctly to avoid variable capture
propSubstitute :: Var -> Term -> Proposition -> Proposition
propSubstitute var replacement prop = go prop
  where
    go (EqualityProp t1 t2) =
      EqualityProp (substitute var replacement t1) (substitute var replacement t2)

    go (UniversalQuantProp v p)
      | v == var = UniversalQuantProp v p  -- Bound variable shadows the substitution
      | v `Set.member` freeVars replacement =
          -- Need to rename v to avoid capture
          let freshVar = freshenPropVar v prop replacement
          in UniversalQuantProp freshVar (propSubstitute var replacement (propAlphaRename v freshVar p))
      | otherwise = UniversalQuantProp v (go p)

    go (ImplicationProp p q) =
      ImplicationProp (go p) (go q)

    go (ConjunctionProp p q) =
      ConjunctionProp (go p) (go q)

    go (TypingProp t ty) =
      TypingProp (substitute var replacement t) ty

    go (PredicateApp name terms) =
      PredicateApp name (map (substitute var replacement) terms)

    go (NegationProp p) =
      NegationProp (go p)

    go (DisjunctionProp p q) =
      DisjunctionProp (go p) (go q)

-- | Alpha-rename a variable in a proposition
propAlphaRename :: Var -> Var -> Proposition -> Proposition
propAlphaRename oldVar newVar = go
  where
    go (EqualityProp t1 t2) =
      EqualityProp (alphaRename oldVar newVar t1) (alphaRename oldVar newVar t2)

    go (UniversalQuantProp v p)
      | v == oldVar = UniversalQuantProp newVar (go p)
      | otherwise = UniversalQuantProp v (go p)

    go (ImplicationProp p q) = ImplicationProp (go p) (go q)
    go (ConjunctionProp p q) = ConjunctionProp (go p) (go q)
    go (TypingProp t ty) = TypingProp (alphaRename oldVar newVar t) ty
    go (PredicateApp name terms) =
      PredicateApp name (map (alphaRename oldVar newVar) terms)
    go (NegationProp p) = NegationProp (go p)
    go (DisjunctionProp p q) = DisjunctionProp (go p) (go q)

-- | Generate a fresh variable for propositions
freshenPropVar :: Var -> Proposition -> Term -> Var
freshenPropVar v prop replacement =
  let occupied = Set.union (propFreeVariables prop) (freeVars replacement)
      Var varName = v
      candidates = map (\n -> Var (varName ++ "_" ++ show n)) [1..]
  in head [c | c <- candidates, c `Set.notMember` occupied]

-- ============================================================================
-- Pretty Printing
-- ============================================================================

-- | Pretty-print a proposition for human-readable output
prettyPrintProposition :: Proposition -> String
prettyPrintProposition (EqualityProp t1 t2) =
  prettyTerm t1 ++ " ≡ " ++ prettyTerm t2

prettyPrintProposition (UniversalQuantProp v prop) =
  "∀ " ++ prettyVar v ++ ". " ++ prettyPrintProposition prop

prettyPrintProposition (ImplicationProp p q) =
  "(" ++ prettyPrintProposition p ++ " → " ++ prettyPrintProposition q ++ ")"

prettyPrintProposition (ConjunctionProp p q) =
  "(" ++ prettyPrintProposition p ++ " ∧ " ++ prettyPrintProposition q ++ ")"

prettyPrintProposition (TypingProp t ty) =
  prettyTerm t ++ " : " ++ ty

prettyPrintProposition (PredicateApp name terms) =
  name ++ "(" ++ intercalate ", " (map prettyTerm terms) ++ ")"

prettyPrintProposition (NegationProp p) =
  "¬(" ++ prettyPrintProposition p ++ ")"

prettyPrintProposition (DisjunctionProp p q) =
  "(" ++ prettyPrintProposition p ++ " ∨ " ++ prettyPrintProposition q ++ ")"

-- | Pretty-print a term (minimal)
prettyTerm :: Term -> String
prettyTerm (TVar v) = prettyVar v
prettyTerm (TAbs v body) = "(λ" ++ prettyVar v ++ ". " ++ prettyTerm body ++ ")"
prettyTerm (TApp f x) = "(" ++ prettyTerm f ++ " " ++ prettyTerm x ++ ")"
prettyTerm (TConst c) = c
prettyTerm (TLet v val body) = "(let " ++ prettyVar v ++ " = " ++ prettyTerm val ++ " in " ++ prettyTerm body ++ ")"
prettyTerm (TPair a b) = "(" ++ prettyTerm a ++ ", " ++ prettyTerm b ++ ")"
prettyTerm (TFst t) = "fst(" ++ prettyTerm t ++ ")"
prettyTerm (TSnd t) = "snd(" ++ prettyTerm t ++ ")"

-- | Pretty-print a variable
prettyVar :: Var -> String
prettyVar (Var name) = name

-- ============================================================================
-- Elaboration Interface
-- ============================================================================

-- | Surface-level assertion (produced by elaborator, Agent 3A)
-- This is the input type for elaboration
data SurfaceAssertion = SurfaceAssertion
  { surfaceName :: String
    -- ^ The assertion name
  , surfacePropositionText :: String
    -- ^ The proposition as text (needs parsing by Agent 3A)
  , surfaceSourceLoc :: SourceLoc
    -- ^ Source location
  }
  deriving (Eq, Show, Generic, Typeable)

-- | Elaboration interface
-- This signature shows how surface assertions are converted to core assertions
-- Implementation of this function is the responsibility of Agent 3A
-- when they implement the parser/elaborator
--
-- Type signature for Agent 3A to implement:
-- elaborateSurfaceAssertion :: SurfaceAssertion -> Either String Assertion
--
-- For now, we provide a stub that always fails with an appropriate message
elaborateAssertion :: SurfaceAssertion -> Either String Assertion
elaborateAssertion surface =
  Left $ "Elaboration not yet implemented. "
      ++ "Agent 3A must implement parsing of: "
      ++ surfacePropositionText surface

-- ============================================================================
-- Integration Interface for Agent 2B (Proof-Term Checker)
-- ============================================================================

{-|
Proof-term checker integration (Agent 2B)

The proof checker receives:
1. A Proposition (the obligation to discharge)
2. A ProofTerm (the evidence for the proposition)

Expected type signature in Agent 2B's module:
  proofChecks :: ProofTerm -> Proposition -> Either String ()

Invariants the proof checker assumes about well-formed propositions:
  - All bound variables are properly scoped (no captures)
  - Free variables are explicitly mentioned in universal quantifications
  - Terms in equality propositions are well-typed (type checking by Agent 3B)
  - Predicate applications reference known predicates
  - No circular references in implication chains

When a proof is invalid or the proposition is malformed, the checker
MUST return a descriptive error message, never silently reject.
-}

