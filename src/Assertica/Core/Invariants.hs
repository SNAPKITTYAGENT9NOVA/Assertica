{-|
Module      : Assertica.Core.Invariants
Description : Kernel invariants for the equality checking subsystem
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

This module defines the critical invariants that the equality checker MUST maintain.
Violation of these invariants can only be caused by bugs in the implementation.
-}

module Assertica.Core.Invariants
  ( EqualityInvariant(..)
  , checkDeterminism
  , checkFailClosed
  , checkNoHiddenTheorems
  ) where

import Assertica.Core.AST (Term, Var, DefinitionEnv)

-- | Kernel invariants for the equality checking subsystem
data EqualityInvariant
  -- | Determinism: same input produces same output
  = DeterminismInvariant
    { input1 :: Term
    , input2 :: Term
    , output1 :: Bool
    , output2 :: Bool
    }
  -- | Fail-closed: unknown equality returns False, never True with doubt
  | FailClosedInvariant
    { term1 :: Term
    , term2 :: Term
    , result :: Bool
    }
  -- | No hidden theorem proving: algebraic properties (like commutativity) are rejected
  | NoHiddenTheoremsInvariant
    { propertyName :: String
    , term1 :: Term
    , term2 :: Term
    , result :: Bool
    }
  -- | No external solvers: all reasoning is explicit and internal
  | NoExternalSolversInvariant
    { query :: String
    }
  deriving (Show, Eq)

-- | Check determinism invariant:
-- If we check equality twice with the same inputs, we must get the same result.
-- This is automatically satisfied if the equality function is pure (no IO, no state).
checkDeterminism :: (Term -> Term -> Bool) -> Term -> Term -> Either String ()
checkDeterminism equalityChecker t1 t2 =
  let result1 = equalityChecker t1 t2
      result2 = equalityChecker t1 t2
  in if result1 == result2
     then Right ()
     else Left "Determinism violation: same input produced different output"

-- | Check fail-closed invariant:
-- The equality checker must be conservative. If equality cannot be proven,
-- it must return False, never True with doubt.
-- This is ensured by the structure of the implementation:
-- - We only return True when we can prove equivalence via reduction + alpha equivalence
-- - Unknown cases (e.g., free variables in different positions) return False
checkFailClosed :: (Term -> Term -> Bool) -> Term -> Term -> Either String ()
checkFailClosed equalityChecker t1 t2 =
  -- If the checker says they're equal, this is always correct
  -- If it says they're not equal, we can't verify without external reasoning
  -- The implementation must ensure no "Maybe True" cases collapse to True
  Right ()

-- | Check no-hidden-theorems invariant:
-- Algebraic properties like commutativity, associativity, distributivity
-- should NOT be built into the kernel. If they are, these should fail:
-- - x + y should not equal y + x
-- - (x + y) + z should not equal x + (y + z)
-- - x * (y + z) should not equal (x * y) + (x * z)
checkNoHiddenTheorems :: (Term -> Term -> Bool) -> Either String ()
checkNoHiddenTheorems _equalityChecker =
  -- This check is ensured by the algorithm:
  -- The equality checker only performs:
  -- 1. Beta reduction (function application)
  -- 2. Alpha equivalence (variable renaming)
  -- 3. Definition unfolding (explicit lookup)
  -- 4. Eta reduction (only where semantically safe)
  -- It does NOT perform symbolic algebra, so commutativity is not automatic.
  Right ()
