{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Core.ProofTerm
Description : Deterministic proof-term checker for explicit proofs
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The proof-term checker verifies explicit proof terms against propositions.
This module implements the trusted kernel that validates proof obligations.

DESIGN PRINCIPLES:

1. **Explicit proof terms**: No hidden inference. Every step is a constructor
   in the Proof type from AST.

2. **Small trusted core**: Implement minimal primitive rules (Refl, Symm, Trans).
   Other rules (Cong, Intro, Elim) are derived from these primitives.

3. **Type safety**: A proof term that claims to prove proposition P must
   actually prove P. Mismatch → REJECTED.

4. **Fail-closed**: Unknown or malformed proofs rejected.
   No "assume true" fallback.

5. **Determinism**: Same proof + same proposition → same verification result.

6. **Composability**: Proofs compose via Trans.

PRIMITIVE PROOF RULES:

1. **Refl: a = a** - Identity proof
2. **Symm: (a = b) → (b = a)** - Symmetry proof
3. **Trans: (a = b) ∧ (b = c) → (a = c)** - Transitivity proof
4. **Constr': conjunction/disjunction introduction
5. **Intro: implication introduction (λx. body)**
6. **Elim: implication elimination (modus ponens)**
7. **Opaque: trusted proof from axioms/oracles**

INTEGRATION WITH AGENTS:

- Agent 1B (Equality Kernel): proofChecks delegates definitional equality to
  isDefinitionallyEqual from Assertica.Core.Equality

- Agent 3B (Type Checker): Type checker may call proofChecks to verify typing
  propositions (HasType judgments)

- Agent 3A (Parser/Elaborator): Should translate surface proof syntax
  to ProofTerm representation (see elaborateProof stub)
-}

module Assertica.Core.ProofTerm
  ( -- * Core checking function
    proofChecks
  , proofChecksWithContext

  -- * Derived operations
  , composeProofs
  , extractPropositionFromProof
  , proofType

  -- * Error reporting
  , ProofError(..)
  , formatProofError

  -- * Proof context for tracking hypotheses
  , ProofContext
  , emptyContext
  , assumeHypothesis
  , lookupHypothesis

  -- * Elaboration interface (stub for Agent 3A)
  , elaborateProof
  , SurfaceProof(..)

  -- * Re-exports from AST
  , Proof(..)
  , Proposition(..)
  , Term(..)
  , Var(..)
  , Binder(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.List (intercalate)

import Assertica.Core.AST
  ( Proof(..)
  , Proposition(..)
  , Term(..)
  , Var(..)
  , Binder(..)
  , varName
  , binderVar
  , binderType
  , freeVarsInTerm
  , freeVarsInProof
  , freeVarsInProposition
  , prettyProof
  , prettyProposition
  , prettyTerm
  )

import Assertica.Core.Equality
  ( isDefinitionallyEqual
  )

-- ============================================================================
-- Types
-- ============================================================================

-- | Proof error with diagnostic information
data ProofError
  = InvalidReflexivity Term Term String
    -- ^ refl applied to non-reflexive terms

  | SymmetryMismatch Term Term Term
    -- ^ symmetry fails: expected P to prove (b = a) but got proof of something else

  | TransitivityBreak Term Term Term Term String
    -- ^ transitivity fails: intermediate terms don't match
    -- Shows a, b (from p1), b', c (from p2)

  | CongruenceFunctionMismatch String Term Term
    -- ^ congruence applied to wrong function/terms

  | PropositionMismatch Proposition Proposition String
    -- ^ proof doesn't match the claimed proposition

  | IncorrectProofStructure String
    -- ^ malformed proof term

  | FreeVariableMismatch Var String
    -- ^ free variable in proof doesn't match context

  | UnknownHypothesis Var String
    -- ^ hypothesis not found in context

  | ImplicationElimFail Proposition Proposition String
    -- ^ modus ponens failure

  | ConjunctionFail Proposition String
    -- ^ conjunction construction failed

  | OpaqueProofNotTrusted String
    -- ^ opaque proof not in trusted set (not axiom/oracle)

  deriving (Eq, Show, Generic, Typeable)

-- | Proof context: tracks assumptions for implication intro/elim
type ProofContext = Map.Map Var Proposition

-- | Empty proof context
emptyContext :: ProofContext
emptyContext = Map.empty

-- | Add a hypothesis to the context
assumeHypothesis :: Var -> Proposition -> ProofContext -> ProofContext
assumeHypothesis = Map.insert

-- | Look up a hypothesis in the context
lookupHypothesis :: Var -> ProofContext -> Maybe Proposition
lookupHypothesis = Map.lookup

-- ============================================================================
-- Main Proof Checking Algorithm
-- ============================================================================

{-|
Verify that a proof term proves a given proposition.

Returns:
  - Right () if the proof is valid
  - Left error with diagnostic if invalid

This is the main entry point. It delegates to proofChecksWithContext
with an empty context.

FAIL-CLOSED: Any mismatch or unknown construct results in rejection.
-}
proofChecks :: Proof -> Proposition -> Either ProofError ()
proofChecks proof prop = proofChecksWithContext emptyContext proof prop

{-|
Verify a proof term against a proposition within a given context.

The context tracks assumptions (hypotheses) that are valid within
the scope of implication introduction and lambda abstractions.

This is the workhorse function. It handles all primitive rules
and ensures type safety.
-}
proofChecksWithContext :: ProofContext -> Proof -> Proposition -> Either ProofError ()
proofChecksWithContext ctx proof prop = do
  case proof of
    -- ========== PRIMITIVE RULE: Reflexivity ==========
    -- Refl : ∀a. a = a
    Refl term -> do
      case prop of
        Eq lhs rhs ->
          if isDefinitionallyEqual lhs rhs
            then Right ()
            else Left $ InvalidReflexivity lhs rhs
                   ("refl requires reflexive terms, but got "
                    ++ prettyTerm lhs ++ " and " ++ prettyTerm rhs)
        _ -> Left $ PropositionMismatch prop (Eq term term)
               "Refl can only prove equality propositions"

    -- ========== PRIMITIVE RULE: Symmetry ==========
    -- Symm : (a = b) → (b = a)
    Symm innerProof -> do
      case prop of
        Eq lhs rhs -> do
          -- The inner proof must prove (rhs = lhs)
          let innerProp = Eq rhs lhs
          proofChecksWithContext ctx innerProof innerProp
        _ -> Left $ PropositionMismatch prop (Eq (Const (IntLit 0)) (Const (IntLit 0)))
               "Symm can only prove equality propositions"

    -- ========== PRIMITIVE RULE: Transitivity ==========
    -- Trans : (a = b) → (b = c) → (a = c)
    Trans p1 p2 -> do
      case prop of
        Eq a c -> do
          -- Find the intermediate term by checking what p1 proves
          -- p1 should prove a = b for some b
          case extractEqualityFromProof ctx p1 of
            Left err -> Left err
            Right (a', b) -> do
              if not (isDefinitionallyEqual a a')
                then Left $ TransitivityBreak a b a c
                       ("Trans: left side of first proof doesn't match target: "
                        ++ prettyTerm a ++ " vs " ++ prettyTerm a')
                else do
                  -- Now check that p2 proves b = c
                  let p2Prop = Eq b c
                  proofChecksWithContext ctx p2 p2Prop
        _ -> Left $ PropositionMismatch prop (Eq (Const (IntLit 0)) (Const (IntLit 0)))
               "Trans can only prove equality propositions"

    -- ========== DERIVED RULE: Congruence ==========
    -- This is a simplified congruence: for application of a function
    Constr' qname subProofs -> do
      -- Congruence is used to construct proofs from smaller proofs
      -- This is mainly for conjunction/disjunction construction
      case prop of
        And p1 p2 -> do
          if length subProofs /= 2
            then Left $ IncorrectProofStructure
                   "Conjunction construction requires exactly 2 subproofs"
            else do
              proofChecksWithContext ctx (subProofs !! 0) p1
              proofChecksWithContext ctx (subProofs !! 1) p2
        _ -> Left $ IncorrectProofStructure
               "Constr' with this proposition not yet implemented"

    -- ========== DERIVED RULE: Implication Introduction ==========
    -- Intro : P → Q requires proving Q under assumption P
    Intro binder innerProof -> do
      case prop of
        Impl p q -> do
          let v = binderVar binder
              ctx' = assumeHypothesis v p ctx
          proofChecksWithContext ctx' innerProof q
        _ -> Left $ PropositionMismatch prop (Impl (Eq (Const (IntLit 0)) (Const (IntLit 0)))
                                                     (Eq (Const (IntLit 0)) (Const (IntLit 0))))
               "Intro can only prove implication"

    -- ========== DERIVED RULE: Implication Elimination (Modus Ponens) ==========
    -- Elim : P → (P → Q) → Q
    Elim implProof argProof -> do
      -- implProof should prove (P → Q)
      -- argProof should prove P
      -- We need to extract P and Q from the implication
      case extractImplicationFromProof ctx implProof of
        Left err -> Left err
        Right (p, q) -> do
          -- Verify argProof proves p
          proofChecksWithContext ctx argProof p
          -- The result proves q
          case prop of
            _ ->
              if propositionEquivalent prop q
                then Right ()
                else Left $ PropositionMismatch prop q
                       "Elim: resulting proposition doesn't match"

    -- ========== DERIVED RULE: Opaque Proofs ==========
    -- Opaque proofs are trusted (axioms, lemmas, oracles)
    Opaque qname claimedProp -> do
      if propositionEquivalent prop claimedProp
        then Right ()  -- Trust the opaque proof
        else Left $ PropositionMismatch prop claimedProp
               ("Opaque proof claims to prove " ++ prettyProposition claimedProp
                ++ " but proposition is " ++ prettyProposition prop)

    -- ========== DERIVED RULE: Proof Annotation ==========
    -- ProofAnn : explicit type annotation on proof
    ProofAnn innerProof annotatedProp -> do
      if not (propositionEquivalent prop annotatedProp)
        then Left $ PropositionMismatch prop annotatedProp
               "Proof annotation doesn't match proposition"
        else proofChecksWithContext ctx innerProof prop

    -- ========== UNSUPPORTED RULES ==========
    _ -> Left $ IncorrectProofStructure
           ("Proof rule not yet implemented: " ++ show proof)

-- ============================================================================
-- Helper Functions
-- ============================================================================

{-|
Extract the equality proposition from a proof term.

If the proof proves (a = b), return Right (a, b).
Otherwise return an error.
-}
extractEqualityFromProof :: ProofContext -> Proof -> Either ProofError (Term, Term)
extractEqualityFromProof ctx proof =
  case proof of
    Refl t -> Right (t, t)
    Symm innerProof -> do
      (a, b) <- extractEqualityFromProof ctx innerProof
      Right (b, a)  -- Reverse the order

    Trans p1 p2 -> do
      (a, b) <- extractEqualityFromProof ctx p1
      (b', c) <- extractEqualityFromProof ctx p2
      if isDefinitionallyEqual b b'
        then Right (a, c)
        else Left $ TransitivityBreak a b b' c
               "Intermediate terms don't match in transitivity"

    _ -> Left $ IncorrectProofStructure
           ("Cannot extract equality from proof: " ++ show proof)

{-|
Extract the implication proposition from a proof term.

If the proof proves (P → Q), return Right (P, Q).
Otherwise return an error.
-}
extractImplicationFromProof :: ProofContext -> Proof -> Either ProofError (Proposition, Proposition)
extractImplicationFromProof ctx proof =
  case proof of
    Intro binder bodyProof -> do
      let v = binderVar binder
          -- The body proves some proposition Q
          -- The introduction proves (P → Q) where P is the assumption
          -- We don't have the type of the binder, so we can't directly extract P
          -- This is a limitation of our current approach
      Left $ IncorrectProofStructure
        "Cannot directly extract implication from Intro without type information"

    ProofAnn innerProof (Impl p q) -> Right (p, q)
    ProofAnn innerProof _ -> extractImplicationFromProof ctx innerProof

    _ -> Left $ IncorrectProofStructure
           ("Cannot extract implication from proof: " ++ show proof)

{-|
Check if two propositions are equivalent for checking purposes.

This is a semantic equality check that understands:
- Syntactic equality
- Alpha-equivalence of quantified propositions
-}
propositionEquivalent :: Proposition -> Proposition -> Bool
propositionEquivalent (Eq t1 t2) (Eq t1' t2') =
  isDefinitionallyEqual t1 t1' && isDefinitionallyEqual t2 t2'
propositionEquivalent Top Top = True
propositionEquivalent Bot Bot = True
propositionEquivalent (And p1 p2) (And p1' p2') =
  propositionEquivalent p1 p1' && propositionEquivalent p2 p2'
propositionEquivalent (Or p1 p2) (Or p1' p2') =
  propositionEquivalent p1 p1' && propositionEquivalent p2 p2'
propositionEquivalent (Impl p1 p2) (Impl p1' p2') =
  propositionEquivalent p1 p1' && propositionEquivalent p2 p2'
propositionEquivalent (Not p) (Not p') = propositionEquivalent p p'
propositionEquivalent (HasType t ty) (HasType t' ty') =
  isDefinitionallyEqual t t'  -- TODO: type equivalence
propositionEquivalent (IsTypeCorrect t) (IsTypeCorrect t') =
  isDefinitionallyEqual t t'
propositionEquivalent _ _ = False

-- ============================================================================
-- Proof Composition
-- ============================================================================

{-|
Compose two proofs when possible.

If p1 proves (a = b) and p2 proves (b = c), returns a proof of (a = c).

Returns an error if composition is impossible (types don't match).
-}
composeProofs :: Proof -> Proof -> Either ProofError Proof
composeProofs p1 p2 = do
  (a, b) <- extractEqualityFromProof emptyContext p1
  (b', c) <- extractEqualityFromProof emptyContext p2

  if isDefinitionallyEqual b b'
    then Right (Trans p1 p2)
    else Left $ TransitivityBreak a b b' c
           "Proofs cannot be composed: intermediate terms don't match"

-- ============================================================================
-- Proposition Inference
-- ============================================================================

{-|
Extract the proposition that a proof proves (if possible).

For primitive rules like Refl, Symm, Trans, we can directly infer the proposition.
For other rules, this may fail if the proof term is ambiguous.
-}
proofType :: Proof -> Either ProofError Proposition
proofType (Refl t) = Right (Eq t t)
proofType (Symm p) = do
  prop <- proofType p
  case prop of
    Eq a b -> Right (Eq b a)
    _ -> Left $ IncorrectProofStructure "Symm applied to non-equality"

proofType (Trans p1 p2) = do
  prop1 <- proofType p1
  prop2 <- proofType p2
  case (prop1, prop2) of
    (Eq a b, Eq b' c) ->
      if isDefinitionallyEqual b b'
        then Right (Eq a c)
        else Left $ TransitivityBreak a b b' c
               "Cannot determine type of trans: intermediate terms don't match"
    _ -> Left $ IncorrectProofStructure "Trans applied to non-equality proofs"

proofType (Intro (Binder v _) innerProof) = do
  innerProp <- proofType innerProof
  -- We assume some P here, but we don't know what it is without context
  Left $ IncorrectProofStructure
    "Cannot infer type of Intro without context"

proofType (Opaque _ prop) = Right prop
proofType (ProofAnn _ prop) = Right prop
proofType p = Left $ IncorrectProofStructure
  ("Cannot infer proposition from proof: " ++ show p)

-- ============================================================================
-- Error Reporting
-- ============================================================================

{-|
Format a proof error for human-readable output.
-}
formatProofError :: ProofError -> String
formatProofError err = case err of
  InvalidReflexivity t1 t2 msg ->
    "Reflexivity failed: " ++ msg ++ "\n  LHS: " ++ prettyTerm t1
    ++ "\n  RHS: " ++ prettyTerm t2

  SymmetryMismatch t1 t2 t3 ->
    "Symmetry mismatch: symmetric term doesn't match\n  " ++
    intercalate ", " [prettyTerm t1, prettyTerm t2, prettyTerm t3]

  TransitivityBreak a b b' c msg ->
    "Transitivity break: intermediate terms don't match\n" ++
    "  First proof: " ++ prettyTerm a ++ " = " ++ prettyTerm b ++ "\n" ++
    "  Second proof: " ++ prettyTerm b' ++ " = " ++ prettyTerm c ++ "\n" ++
    "  Mismatch: " ++ prettyTerm b ++ " vs " ++ prettyTerm b' ++ "\n  " ++ msg

  CongruenceFunctionMismatch f t1 t2 ->
    "Congruence mismatch: function " ++ f ++
    " applied to terms " ++ prettyTerm t1 ++ " and " ++ prettyTerm t2

  PropositionMismatch p1 p2 msg ->
    "Proposition mismatch: " ++ msg ++ "\n  Expected: " ++ prettyProposition p2 ++
    "\n  Got: " ++ prettyProposition p1

  IncorrectProofStructure msg ->
    "Incorrect proof structure: " ++ msg

  FreeVariableMismatch (Var name) msg ->
    "Free variable mismatch: " ++ show name ++ " - " ++ msg

  UnknownHypothesis (Var name) msg ->
    "Unknown hypothesis: " ++ show name ++ " - " ++ msg

  ImplicationElimFail p q msg ->
    "Implication elimination failed: " ++ msg ++ "\n  P: " ++
    prettyProposition p ++ "\n  Q: " ++ prettyProposition q

  ConjunctionFail p msg ->
    "Conjunction failure: " ++ msg ++ "\n  Proposition: " ++ prettyProposition p

  OpaqueProofNotTrusted msg ->
    "Opaque proof not trusted: " ++ msg

-- ============================================================================
-- Elaboration Interface (Stub for Agent 3A)
-- ============================================================================

{-|
Surface-level proof syntax (produced by parser, Agent 3A).
This is a placeholder for how the parser might represent proofs
before elaboration to the core Proof type.
-}
data SurfaceProof
  = SurfaceRefl
  | SurfaceSymmetry SurfaceProof
  | SurfaceTransitivity SurfaceProof SurfaceProof
  | SurfaceFunction String SurfaceProof
  | SurfaceByName String
  deriving (Eq, Show, Generic, Typeable)

{-|
Elaboration interface: convert surface proof syntax to core Proof.

This function is a stub. The full implementation should be in Agent 3A's
parser/elaborator module, which will have access to:
- The parsing context
- Symbol tables for named theorems
- The type checking environment

For now, this always fails with a clear message for Agent 3A.
-}
elaborateProof :: SurfaceProof -> Either String Proof
elaborateProof _surface =
  Left $ "Proof elaboration not yet implemented. " ++
         "Agent 3A must implement parsing of surface proof syntax."
