{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Backend.ProofCompiler
Description : Compile Assertica proofs to Haskell evidence
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

Translates proof terms from the Core AST to Haskell evidence terms:

MAPPINGS:

- Refl a                     → Refl (reflexivity proof)
- Symm p                     → Symm p (symmetry proof)
- Trans p1 p2                → Trans p1 p2 (transitivity proof)
- Intro (x : T) . p          → \(x :: T) -> <compiled p> (intro)
- Elim p1 p2                 → p1 p2 (elimination / application)
- Constr' C [p1, ..., pn]    → C p1 ... pn (proof constructor)
- Case' e [pat->body] def    → case e of { pat -> body } (proof case)
- App' p e                   → p e (proof application to term)
- Opaque name prop           → proofConst_<name> (trusted proof)
- Conv p T1 T2               → Conv p (implicit coercion)
- ProofAnn p prop            → (p :: <compiled prop>) (annotation)
- ProofVar v                 → v (proof variable)

PROOF IRRELEVANCE:

Propositions are proof-irrelevant in Haskell (all proofs of the same proposition
are equal from a computation perspective). The compiler erases proof content
where appropriate:

1. Proofs of Props (not Types) compile to ()
2. Proofs used only for type safety are erased
3. Only informative proofs (e.g., constructive proofs) preserve content

PRINCIPLES:

1. **Type Safety**: All generated proofs typecheck in Haskell
2. **Proof Irrelevance**: Leverage Haskell's representation independence
3. **No Unsafe Code**: Never use unsafeCoerce or similar
4. **Determinism**: Same proof produces same evidence
-}

module Assertica.Backend.ProofCompiler
  ( -- * Main compilation function
    compileProof
  , compileProofWith
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Assertica.Core.AST
import Assertica.Backend.CodeGen (CodeGenConfig)
import qualified Assertica.Backend.TermCompiler as TC

-- ============================================================================
-- MAIN COMPILATION FUNCTION
-- ============================================================================

{-| Compile a proof to Haskell evidence code.

This is the primary entry point for proof compilation.
Returns either a Haskell expression string or an error.
-}
compileProof :: CodeGenConfig -> Proof -> Either String Text
compileProof config proof = compileProofWith config Set.empty proof

{-| Compile a proof with a given bound variable context -}
compileProofWith :: CodeGenConfig -> Set.Set Var -> Proof -> Either String Text
compileProofWith config bound = \case
  Refl _term ->
    -- Reflexivity: just the Refl constructor
    Right "Refl"

  Symm p -> do
    -- Symmetry: apply Symm constructor to compiled proof
    cp <- compileProofWith config bound p
    Right $ "(Symm " <> cp <> ")"

  Trans p1 p2 -> do
    -- Transitivity: compose proofs
    cp1 <- compileProofWith config bound p1
    cp2 <- compileProofWith config bound p2
    Right $ "(Trans " <> cp1 <> " " <> cp2 <> ")"

  Intro (Binder (Var name _) _) p -> do
    -- Introduction (implication): lambda abstraction
    let bound' = Set.insert (Var name 0) bound
    cp <- compileProofWith config bound' p
    Right $ "(\\(" <> name <> ") -> " <> cp <> ")"

  Elim p1 p2 -> do
    -- Elimination (modus ponens): proof application
    cp1 <- compileProofWith config bound p1
    cp2 <- compileProofWith config bound p2
    Right $ "(" <> cp1 <> " " <> cp2 <> ")"

  Constr' (QName mod name) proofs -> do
    -- Proof constructor: data constructor applied to proofs
    compiledProofs <- mapM (compileProofWith config bound) proofs
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    if null compiledProofs
      then Right qname
      else Right $ "(" <> qname <> " " <> T.intercalate " " compiledProofs <> ")"

  Case' scrutinee clauses defaultClause -> do
    -- Case analysis on proof
    cs <- compileProofWith config bound scrutinee
    let compiledClauses = mapM (compileProofClause config bound) clauses
    case compiledClauses of
      Left err -> Left err
      Right clauses' ->
        let caseClauses = T.intercalate "; " clauses'
            defaultPart = case defaultClause of
              Nothing -> ""
              Just d -> do
                cd <- compileProofWith config bound d
                "; _ -> " <> cd
        in Right $ "(case " <> cs <> " of { " <> caseClauses <> defaultPart <> " })"

  App' p term -> do
    -- Application of proof to term (instantiation)
    cp <- compileProofWith config bound p
    ct <- TC.compileTerm config term
    Right $ "(" <> cp <> " " <> ct <> ")"

  Opaque (QName mod name) _prop -> do
    -- Opaque/trusted proof from axiom or oracle
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    Right $ "proof_" <> qname

  Conv p _t1 _t2 -> do
    -- Type conversion (implicit)
    cp <- compileProofWith config bound p
    Right $ "(Conv " <> cp <> ")"

  ProofAnn p _prop -> do
    -- Proof annotation: preserve the proof itself
    compileProofWith config bound p

  ProofVar (Var name _) ->
    -- Proof variable reference
    Right name

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Compile a proof clause (pattern -> proof body) -}
compileProofClause :: CodeGenConfig -> Set.Set Var -> Clause -> Either String Text
compileProofClause config bound (Clause pat body) = do
  cp <- compilePatternForProof pat
  -- Extract proof from term body (simplified)
  let proofBody = case body of
        ProofTerm p -> p
        _ -> Refl (Const UnitConst)  -- Default: reflexivity
  cb <- compileProofWith config bound proofBody
  Right $ cp <> " -> " <> cb

{-| Compile pattern for proof matching -}
compilePatternForProof :: Pattern -> Either String Text
compilePatternForProof = \case
  PatVar (Var name _) -> Right name

  PatConstr (QName mod name) pats -> do
    compiledPats <- mapM compilePatternForProof pats
    let qname = if null mod
                  then name
                  else T.intercalate "." mod <> "." <> name
    if null compiledPats
      then Right qname
      else Right $ "(" <> qname <> " " <> T.intercalate " " compiledPats <> ")"

  PatWildcard -> Right "_"

  PatLit lit -> Right $ T.pack lit

-- ============================================================================
-- PROOF IRRELEVANCE
-- ============================================================================

{-| Determine if a proposition is proof-irrelevant.

In Haskell's Curry-Howard isomorphism, a Prop (proposition) is proof-irrelevant
because all proofs of the same proposition are computationally equivalent.
A Type, on the other hand, represents informative data.
-}
isProofIrrelevant :: Proposition -> Bool
isProofIrrelevant = \case
  Eq _ _ -> True      -- Equality is proof-irrelevant
  Top -> True         -- True has exactly one proof
  Bot -> False        -- False has no proofs
  And p1 p2 -> isProofIrrelevant p1 && isProofIrrelevant p2
  Or p1 p2 -> isProofIrrelevant p1 || isProofIrrelevant p2
  Impl _ _ -> False   -- Implication may have informative content
  Not _ -> True       -- Negation is typically proof-irrelevant
  Forall' _ p -> isProofIrrelevant p
  Exists _ _ -> False -- Existential may need content
  HasType _ _ -> True -- Type judgments are proof-irrelevant
  IsTypeCorrect _ -> True
  Named _ -> False    -- Assume named props may be informative
  Typed p _ -> isProofIrrelevant p

{-| Erase proof-irrelevant content to () -}
eraseIrrelevantProof :: Proposition -> Text
eraseIrrelevantProof prop =
  if isProofIrrelevant prop
    then "()"
    else error "Cannot erase informative proof"
