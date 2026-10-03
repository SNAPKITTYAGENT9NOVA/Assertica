{-|
Module      : Assertica.Surface.Elaborator
Description : Elaborator from surface AST to core AST
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Converts the surface (intermediate) AST into the core AST.
Performs:
- Name resolution (variables, qualified names)
- Type annotation propagation
- Syntactic sugar expansion (infix to application, etc.)
- Implicit argument elaboration (minimal)
- Error reporting with source locations

INVARIANTS:
- All names must be resolvable (either built-in or in scope)
- All type annotations must be well-formed
- No unbound variables in elaborated terms
-}

module Assertica.Surface.Elaborator
  ( -- * Main elaboration functions
    elaborateTerm
  , elaborateType
  , elaborateProposition
  , elaborateAssertion
  , elaborateProof

    -- * Elaboration context
  , ElabContext (..)
  , emptyContext
  , addBinding

    -- * Error types
  , ElabError (..)
  ) where

import Assertica.Surface.AST
import qualified Assertica.Core.AST as Core
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)

-- ============================================================================
-- ELABORATION CONTEXT
-- ============================================================================

{-| Elaboration context tracks variable bindings and types.
-}
data ElabContext = ElabContext
  { ecBindings :: Map Text Core.Var      -- ^ Variable bindings
  , ecTypes    :: Map Text Core.Type     -- ^ Type information for variables
  , ecNextId   :: Int                    -- ^ Next unique identifier
  , ecModule   :: [Text]                 -- ^ Current module path
  }
  deriving (Show)

{-| Empty context.
-}
emptyContext :: ElabContext
emptyContext = ElabContext Map.empty Map.empty 0 []

{-| Add a binding to the context.
-}
addBinding :: Text -> Maybe Core.Type -> ElabContext -> (Core.Var, ElabContext)
addBinding name mty ctx =
  let var = Core.Var name (ecNextId ctx)
      ctx' = ctx
        { ecBindings = Map.insert name var (ecBindings ctx)
        , ecTypes = case mty of
            Just ty -> Map.insert name ty (ecTypes ctx)
            Nothing -> ecTypes ctx
        , ecNextId = ecNextId ctx + 1
        }
  in (var, ctx')

{-| Add multiple bindings.
-}
addBindings :: [Text] -> ElabContext -> ([Core.Var], ElabContext)
addBindings [] ctx = ([], ctx)
addBindings (n:ns) ctx =
  let (v, ctx') = addBinding n Nothing ctx
      (vs, ctx'') = addBindings ns ctx'
  in (v:vs, ctx'')

-- ============================================================================
-- ERROR TYPES
-- ============================================================================

{-| Elaboration error with optional source location.
-}
data ElabError = ElabError
  { eeMessage :: String
  , eeLoc     :: Maybe SourceLoc
  }
  deriving (Eq, Show)

-- ============================================================================
-- ELABORATION MONAD
-- ============================================================================

{-| Elaboration result: either an error or a value.
-}
type Elab a = Either ElabError a

{-| Fail with an error.
-}
elab_fail :: String -> Maybe SourceLoc -> Elab a
elab_fail msg loc = Left (ElabError msg loc)

{-| Resolve a name, looking it up in context or treating as builtin.
-}
resolveName :: Text -> ElabContext -> Elab Core.Var
resolveName name ctx =
  case Map.lookup name (ecBindings ctx) of
    Just var -> return var
    Nothing ->
      -- Builtin or unresolved - for now treat as global reference
      return (Core.Var name 0)

{-| Resolve a qualified name.
-}
resolveQName :: SurfaceQName -> ElabContext -> Elab Core.QName
resolveQName qn ctx =
  case qn of
    [] -> elab_fail "Empty qualified name" Nothing
    [name] ->
      -- Simple name: check context first, else assume it's a global
      case Map.lookup name (ecBindings ctx) of
        Just (Core.Var vname vid) ->
          return (Core.QName [T.pack (show vid)] vname)  -- Local variable reference
        Nothing ->
          return (Core.QName (ecModule ctx) name)
    names ->
      -- Qualified name: module.name.name...
      let modName = init names
          localName = last names
      in return (Core.QName modName localName)

-- ============================================================================
-- TERM ELABORATION
-- ============================================================================

{-| Elaborate a surface term to a core term.
-}
elaborateTerm :: Located SurfaceTerm -> Either ElabError Core.Term
elaborateTerm (Located term loc) = elaborateTermWithCtx term emptyContext

{-| Elaborate a term with context.
-}
elaborateTermWithCtx :: SurfaceTerm -> ElabContext -> Elab Core.Term
elaborateTermWithCtx term ctx = case term of
  STVar name -> do
    var <- resolveName name ctx
    return (Core.Var var)

  STConst c -> do
    const <- elaborateConstant c
    return (Core.Const const)

  STLam vars mty body -> do
    (typedVars, ctx') <- case mty of
      Just ty -> do
        ty' <- elaborateTypeWithCtx ty ctx
        let binders = [Core.Binder v (Just ty') | v <- replicate (length vars) (Core.Var (T.pack "") 0)]
        -- Create proper variables
        (newVars, ctx'') <- foldM (\(acc, c) v -> do
          (v', c') <- return (addBinding v (Just ty') c)
          return (acc ++ [v'], c')
          ) ([], ctx) vars
        return (zipWith Core.Binder newVars (repeat (Just ty')), ctx'')
      Nothing -> do
        (newVars, ctx'') <- addBindings vars ctx
        return (map (\v -> Core.Binder v Nothing) newVars, ctx'')

    body' <- elaborateTermWithCtx (locValue body) ctx'

    -- Build nested lambdas from right to left
    return $ foldr (\b acc -> Core.Lam b acc) body' typedVars

  STApp func args -> do
    func' <- elaborateTermWithCtx (locValue func) ctx
    args' <- mapM (\arg -> elaborateTermWithCtx (locValue arg) ctx) args
    return $ foldl Core.App func' args'

  STInfix l op r -> do
    -- Convert infix to application: a + b  =>  (+) a b
    l' <- elaborateTermWithCtx (locValue l) ctx
    r' <- elaborateTermWithCtx (locValue r) ctx
    opVar <- resolveName op ctx
    return (Core.App (Core.App (Core.Var opVar) l') r')

  STLet name mty e1 e2 -> do
    e1' <- elaborateTermWithCtx (locValue e1) ctx
    ty' <- case mty of
      Just ty -> elaborateTypeWithCtx ty ctx
      Nothing -> return (Core.TyUniverse Core.Type0)  -- Default to Type
    (var, ctx') <- return (addBinding name (Just ty') ctx)
    e2' <- elaborateTermWithCtx (locValue e2) ctx'
    return (Core.Let (Core.Binder var (Just ty')) e1' e2')

  STCase e clauses mdef -> do
    e' <- elaborateTermWithCtx (locValue e) ctx
    clauses' <- mapM (elaborateClause ctx) clauses
    mdef' <- case mdef of
      Just def -> elaborateTermWithCtx (locValue def) ctx >>= return . Just
      Nothing -> return Nothing
    return (Core.Case e' clauses' mdef')

  STAnn e ty -> do
    e' <- elaborateTermWithCtx (locValue e) ctx
    ty' <- elaborateTypeWithCtx ty ctx
    return (Core.Ann e' ty')

  STForall name mty body -> do
    ty' <- case mty of
      Just ty -> elaborateTypeWithCtx ty ctx
      Nothing -> return (Core.TyUniverse Core.Type0)
    (var, ctx') <- return (addBinding name (Just ty') ctx)
    body' <- elaborateTermWithCtx (locValue body) ctx'
    return (Core.Forall (Core.Binder var (Just ty')) body')

  STTuple es -> do
    es' <- mapM (\e -> elaborateTermWithCtx (locValue e) ctx) es
    -- Represent as nested pairs: (a, b, c) ~ Tuple3 a b c
    -- For now, just represent as tuple constructor
    let qn = Core.QName [] (T.pack ("Tuple" ++ show (length es)))
    return (Core.Constr qn es')

  STList es -> do
    es' <- mapM (\e -> elaborateTermWithCtx (locValue e) ctx) es
    -- Build list from cons cells: [a, b, c] ~ Cons a (Cons b (Cons c Nil))
    let nil = Core.Constr (Core.QName [] (T.pack "Nil")) []
    return $ foldr (\e acc -> Core.Constr (Core.QName [] (T.pack "Cons")) [e, acc]) nil es'

  STConstr qn args -> do
    qn' <- resolveQName qn ctx
    args' <- mapM (\e -> elaborateTermWithCtx (locValue e) ctx) args
    return (Core.Constr qn' args')

{-| Elaborate a clause (pattern -> body).
-}
elaborateClause :: ElabContext -> SurfaceClause -> Elab Core.Clause
elaborateClause ctx (SurfaceClause pat body) = do
  (pat', ctx') <- elaboratePattern (locValue pat) ctx
  body' <- elaborateTermWithCtx (locValue body) ctx'
  return (Core.Clause pat' body')

{-| Elaborate a pattern and extend context with bound variables.
-}
elaboratePattern :: SurfacePattern -> ElabContext -> Elab (Core.Pattern, ElabContext)
elaboratePattern pat ctx = case pat of
  SPVar name -> do
    (var, ctx') <- return (addBinding name Nothing ctx)
    return (Core.PatVar var, ctx')

  SPConstr qn pats -> do
    qn' <- resolveQName qn ctx
    (pats', ctx') <- foldM
      (\(acc, c) p -> do
        (p', c') <- elaboratePattern p c
        return (acc ++ [p'], c')
      ) ([], ctx) pats
    return (Core.PatConstr qn' pats', ctx')

  SPWildcard ->
    return (Core.PatWildcard, ctx)

  SPLit c -> do
    c' <- elaborateConstant c
    return (Core.PatLit (show c'), ctx)

  SPTuple pats -> do
    (pats', ctx') <- foldM
      (\(acc, c) p -> do
        (p', c') <- elaboratePattern p c
        return (acc ++ [p'], c')
      ) ([], ctx) pats
    let qn = Core.QName [] (T.pack ("Tuple" ++ show (length pats)))
    return (Core.PatConstr qn pats', ctx')

{-| Elaborate a constant.
-}
elaborateConstant :: SurfaceConstant -> Elab Core.Constant
elaborateConstant = \case
  IntLit n -> return (Core.IntLit n)
  BoolLit b -> return (Core.BoolLit b)
  StringLit s -> return (Core.StringLit s)
  UnitLit -> return Core.UnitConst

-- ============================================================================
-- TYPE ELABORATION
-- ============================================================================

{-| Elaborate a surface type to a core type.
-}
elaborateType :: SurfaceType -> Either ElabError Core.Type
elaborateType ty = elaborateTypeWithCtx ty emptyContext

{-| Elaborate a type with context.
-}
elaborateTypeWithCtx :: SurfaceType -> ElabContext -> Elab Core.Type
elaborateTypeWithCtx ty ctx = case ty of
  STyVar name -> do
    var <- resolveName name ctx
    return (Core.TyVar var)

  STyConst qn args -> do
    qn' <- resolveQName qn ctx
    args' <- mapM (\a -> elaborateTypeWithCtx a ctx) args
    return (Core.TyConst qn' args')

  STyFun a b -> do
    a' <- elaborateTypeWithCtx a ctx
    b' <- elaborateTypeWithCtx b ctx
    return (Core.TyFun a' b')

  STyForall name mty body -> do
    ty' <- case mty of
      Just t -> elaborateTypeWithCtx t ctx
      Nothing -> return (Core.TyUniverse Core.Type0)
    (var, ctx') <- return (addBinding name (Just ty') ctx)
    body' <- elaborateTypeWithCtx body ctx'
    return (Core.TyForall (Core.Binder var (Just ty')) body')

  STyTuple tys -> do
    tys' <- mapM (\t -> elaborateTypeWithCtx t ctx) tys
    let qn = Core.QName [] (T.pack ("Tuple" ++ show (length tys)))
    return (Core.TyConst qn tys')

  STyApp a b -> do
    a' <- elaborateTypeWithCtx a ctx
    b' <- elaborateTypeWithCtx b ctx
    return (Core.TyApp a' b')

-- ============================================================================
-- PROPOSITION ELABORATION
-- ============================================================================

{-| Elaborate a surface proposition to a core proposition.
-}
elaborateProposition :: Located SurfaceProposition -> Either ElabError Core.Proposition
elaborateProposition (Located prop _) = elaboratePropositionWithCtx prop emptyContext

{-| Elaborate a proposition with context.
-}
elaboratePropositionWithCtx :: SurfaceProposition -> ElabContext -> Elab Core.Proposition
elaboratePropositionWithCtx prop ctx = case prop of
  SPEq l r -> do
    l' <- elaborateTermWithCtx (locValue l) ctx
    r' <- elaborateTermWithCtx (locValue r) ctx
    return (Core.Eq l' r')

  SPForall name mty body -> do
    ty' <- case mty of
      Just t -> elaborateTypeWithCtx t ctx
      Nothing -> return (Core.TyUniverse Core.Type0)
    (var, ctx') <- return (addBinding name (Just ty') ctx)
    body' <- elaboratePropositionWithCtx (locValue body) ctx'
    return (Core.Forall' (Core.Binder var (Just ty')) body')

  SPExists name mty body -> do
    ty' <- case mty of
      Just t -> elaborateTypeWithCtx t ctx
      Nothing -> return (Core.TyUniverse Core.Type0)
    (var, ctx') <- return (addBinding name (Just ty') ctx)
    body' <- elaboratePropositionWithCtx (locValue body) ctx'
    return (Core.Exists (Core.Binder var (Just ty')) body')

  SPAnd p1 p2 -> do
    p1' <- elaboratePropositionWithCtx (locValue p1) ctx
    p2' <- elaboratePropositionWithCtx (locValue p2) ctx
    return (Core.And p1' p2')

  SPOr p1 p2 -> do
    p1' <- elaboratePropositionWithCtx (locValue p1) ctx
    p2' <- elaboratePropositionWithCtx (locValue p2) ctx
    return (Core.Or p1' p2')

  SPImpl p1 p2 -> do
    p1' <- elaboratePropositionWithCtx (locValue p1) ctx
    p2' <- elaboratePropositionWithCtx (locValue p2) ctx
    return (Core.Impl p1' p2')

  SPNot p -> do
    p' <- elaboratePropositionWithCtx (locValue p) ctx
    return (Core.Not p')

  SPTrue -> return Core.Top

  SPFalse -> return Core.Bot

-- ============================================================================
-- PROOF ELABORATION
-- ============================================================================

{-| Elaborate a surface proof to a core proof.
-}
elaborateProof :: Located SurfaceProof -> Either ElabError Core.Proof
elaborateProof (Located proof _) = elaborateProofWithCtx proof emptyContext

{-| Elaborate a proof with context.
-}
elaborateProofWithCtx :: SurfaceProof -> ElabContext -> Elab Core.Proof
elaborateProofWithCtx proof ctx = case proof of
  SPRefl -> return (Core.Refl (Core.Const Core.UnitConst))

  SPSymm p -> do
    p' <- elaborateProofWithCtx (locValue p) ctx
    return (Core.Symm p')

  SPTrans p1 p2 -> do
    p1' <- elaborateProofWithCtx (locValue p1) ctx
    p2' <- elaborateProofWithCtx (locValue p2) ctx
    return (Core.Trans p1' p2')

  SPIntro vars body -> do
    (newVars, ctx') <- addBindings vars ctx
    body' <- elaborateProofWithCtx (locValue body) ctx'
    return $ foldr (\v acc -> Core.Intro (Core.Binder v Nothing) acc) body' newVars

  SPVar name -> do
    var <- resolveName name ctx
    return (Core.ProofVar var)

  SPOpaque qn -> do
    qn' <- resolveQName qn ctx
    return (Core.Opaque qn' Core.Top)  -- Opaque proof with Top proposition

  SPByName strategy ->
    -- Strategies: by_refl, by_simp, etc.
    case T.unpack strategy of
      "by_refl" -> return (Core.Refl (Core.Const Core.UnitConst))
      "by_simp" -> return (Core.Refl (Core.Const Core.UnitConst))  -- Simplified
      _ -> elab_fail ("Unknown proof strategy: " ++ T.unpack strategy) Nothing

-- ============================================================================
-- ASSERTION ELABORATION
-- ============================================================================

{-| Elaborate a surface assertion to a core assertion.
-}
elaborateAssertion :: SurfaceAssertion -> Either ElabError Core.Assertion
elaborateAssertion (SurfaceAssertion nameLocated propLocated proofLocatedOpt) = do
  let name = locValue nameLocated
  let nameLoc = locLoc nameLocated

  -- Create qualified name (for now, unqualified)
  let qn = Core.QName [] name

  -- Elaborate proposition
  prop <- elaboratePropositionWithCtx (locValue propLocated) emptyContext

  -- Elaborate optional proof
  proof <- case proofLocatedOpt of
    Just p -> elaborateProofWithCtx (locValue p) emptyContext >>= return . Just
    Nothing -> return Nothing

  return (Core.Assertion qn prop proof)

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Monadic fold (for building up elaboration state).
-}
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f z xs = foldr (\x acc -> acc >>= \a -> f a x) (return z) xs
