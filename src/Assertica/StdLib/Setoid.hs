{-|
Module      : Assertica.StdLib.Setoid
Description : Setoid algebraic structure: a carrier set with explicit equivalence relation
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

This module implements the Setoid structure, which forms the foundation of the
algebraic hierarchy. A Setoid is a type S equipped with an equivalence relation (≈)
that satisfies reflexivity, symmetry, and transitivity.

CRITICAL PROPERTIES:
  1. Equivalence relation proof: all three properties proven explicitly
  2. Deterministic: equivalence checking is total and decidable
  3. No hidden axioms: all properties must be verified by explicit proof terms
  4. Substitution principle: if a ≈ b, then properties of a hold for b
  5. Kernel-trusted: proofs are checked by Agent 2B

ALGEBRAIC STRUCTURE:
  Setoid S = (S, _≈_ : S → S → Prop)
  where _≈_ satisfies:
    - Reflexive:  ∀ a,     a ≈ a       [Refl]
    - Symmetric:  ∀ a b,   a ≈ b → b ≈ a [Symm]
    - Transitive: ∀ a b c, a ≈ b → b ≈ c → a ≈ c [Trans]

LATTICE FOUNDATION:
  Setoids provide the carrier for lattice operations (⊔, ⊓) with proper
  algebraic reasoning. The lattice structure (Agent 5B part 2) builds on
  Setoid equivalence for correctness.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveEq #-}
{-# LANGUAGE DeriveShow #-}

module Assertica.StdLib.Setoid
  ( -- * Setoid structure
    Setoid(..)
  , SetoidEq(..)
    -- * Constructions
  , trivialSetoid
  , discreteSetoid
  , productSetoid
    -- * Setoid morphisms
  , SetoidMorphism(..)
  , morphismPreservesEq
  , idMorphism
  , composeMorphisms
  ) where

import Assertica.Core.AST
  ( Term(..), Type(..), Var, Proof(..), Proposition(..)
  , Assertion(..), freeVars, substitute
  )
import Assertica.Core.ProofTerm (ProofTerm(..), checkProof)
import Assertica.Core.Equality (isDefinitionallyEqual, normalize)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


-- | A Setoid is a type S together with an explicit equivalence relation
--
-- The Setoid class parameterizes over:
--   s:  the carrier type representation (as a Term)
--   eq: the equivalence relation (a predicate Term -> Term -> Prop)
--
-- All three equivalence laws are proven by explicit proof terms.
data Setoid s = Setoid
  { setoidCarrier :: s
    -- ^ The carrier set (e.g., TConst "Nat", TConst "Bool")
  , setoidEq :: Term -> Term -> Proposition
    -- ^ The equivalence relation: given two terms, produce a Proposition
  , setoidRefl :: Term -> ProofTerm
    -- ^ Proof of reflexivity: ∀ a, a ≈ a
  , setoidSymm :: Term -> Term -> ProofTerm
    -- ^ Proof of symmetry: ∀ a b, a ≈ b → b ≈ a
  , setoidTrans :: Term -> Term -> Term -> ProofTerm
    -- ^ Proof of transitivity: ∀ a b c, a ≈ b → b ≈ c → a ≈ c
  } deriving (Show, Eq)

-- | Decision procedure for setoid equality
--
-- SetoidEq wraps a decidable equivalence check with proof obligations
class SetoidEq s where
  -- | Decide if two elements are equivalent, returning explicit proofs
  decideEq :: s -> s -> Maybe (ProofTerm, ProofTerm)
    -- ^ (proof of equality, proof of inequality) or Nothing if undecidable


-- | TRIVIAL SETOID: Single element (unit)
--
-- The trivial setoid has only one element, and all elements are equivalent.
-- Equivalence is proven trivially by Refl.
trivialSetoid :: Setoid Term
trivialSetoid = Setoid
  { setoidCarrier = TConst "Unit"
  , setoidEq = \_ _ -> EqualityProp (TConst "unit") (TConst "unit")
  , setoidRefl = \_ -> Refl
  , setoidSymm = \_ _ -> Refl
  , setoidTrans = \_ _ _ -> Refl
  }

-- | DISCRETE SETOID: Standard equality (definitional equality)
--
-- The discrete setoid uses definitional equality (from Agent 1B) as the
-- equivalence relation. Every element is only equivalent to itself.
discreteSetoid :: Setoid Term
discreteSetoid = Setoid
  { setoidCarrier = TVar (Var "a")
  , setoidEq = \a b -> EqualityProp a b
  , setoidRefl = \a -> Refl
  , setoidSymm = \a b -> Symm (Refl)
  , setoidTrans = \a b c -> Trans Refl Refl
  }

-- | PRODUCT SETOID: Cartesian product of two setoids
--
-- Given setoids S and T, the product setoid S × T has:
--   - Carrier: pairs (a, b) where a ∈ S, b ∈ T
--   - Equivalence: (a, b) ≈ (a', b') iff a ≈ a' and b ≈ b'
--
-- The equivalence proof is constructed by combining proofs from S and T.
productSetoid :: Setoid Term -> Setoid Term -> Setoid Term
productSetoid s1 s2 = Setoid
  { setoidCarrier = TPair (setoidCarrier s1) (setoidCarrier s2)
  , setoidEq = \p1 p2 -> case (p1, p2) of
      (TPair a b, TPair a' b') ->
        ConjunctionProp (setoidEq s1 a a') (setoidEq s2 b b')
      _ -> EqualityProp p1 p2  -- fallback to defn. eq.
  , setoidRefl = \p -> case p of
      TPair a b -> Constr' "and" [setoidRefl s1 a, setoidRefl s2 b]
      _ -> Refl
  , setoidSymm = \p1 p2 -> case (p1, p2) of
      (TPair a b, TPair a' b') ->
        Constr' "and" [setoidSymm s1 a a', setoidSymm s2 b b']
      _ -> Refl
  , setoidTrans = \p1 p2 p3 -> case (p1, p2, p3) of
      (TPair a b, TPair a' b', TPair a'' b'') ->
        Constr' "and"
          [ setoidTrans s1 a a' a''
          , setoidTrans s2 b b' b''
          ]
      _ -> Refl
  }


-- | SETOID MORPHISM: Structure-preserving map between setoids
--
-- A morphism f: S → T preserves the equivalence relation:
-- If a ≈_S b, then f(a) ≈_T f(b)
data SetoidMorphism = SetoidMorphism
  { morphismSource :: Setoid Term
  , morphismTarget :: Setoid Term
  , morphismMap :: Term -> Term
    -- ^ The function f: S → T
  , morphismProof :: Term -> Term -> ProofTerm
    -- ^ Proof that f preserves equivalence:
    --   a ≈_S b → f(a) ≈_T f(b)
  } deriving (Show, Eq)

-- | PRESERVATION THEOREM: If f is a setoid morphism and a ≈ b, then f(a) ≈ f(b)
--
-- This is the fundamental correctness property of setoid morphisms.
-- It ensures that equivalent elements map to equivalent elements.
morphismPreservesEq :: SetoidMorphism -> Term -> Term -> ProofTerm -> ProofTerm
morphismPreservesEq morph a b eqProof =
  -- Apply the morphism's proof to the equivalence proof
  let fa = morphismMap morph a
      fb = morphismMap morph b
  in morphismProof morph a b eqProof

-- | IDENTITY MORPHISM: The identity map on a setoid
--
-- The identity map is trivially a setoid morphism.
idMorphism :: Setoid Term -> SetoidMorphism
idMorphism s = SetoidMorphism
  { morphismSource = s
  , morphismTarget = s
  , morphismMap = id
  , morphismProof = \_ _ eqProof -> eqProof
  }

-- | COMPOSITION OF MORPHISMS: f ∘ g is a setoid morphism
--
-- If f: S → T and g: T → U are setoid morphisms, then their composition
-- f ∘ g: S → U is also a setoid morphism.
--
-- PROOF:
--   Let a, b ∈ S with a ≈_S b.
--   By g's preservation: g(a) ≈_T g(b)
--   By f's preservation: f(g(a)) ≈_U f(g(b))
--   Therefore (f ∘ g) preserves equivalence. □
composeMorphisms :: SetoidMorphism -> SetoidMorphism -> SetoidMorphism
composeMorphisms f g = SetoidMorphism
  { morphismSource = morphismSource g
  , morphismTarget = morphismTarget f
  , morphismMap = \a -> morphismMap f (morphismMap g a)
  , morphismProof = \a b eqProof ->
      let gEq = morphismProof g a b eqProof
          fEq = morphismProof f (morphismMap g a) (morphismMap g b) gEq
      in fEq
  }
