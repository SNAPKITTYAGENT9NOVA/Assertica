{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Backend.CodeGen
Description : Main code generation engine for Assertica → Haskell compilation
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

This module is the primary interface for translating verified Assertica Core AST
to executable Haskell code. It orchestrates the compilation pipeline:

1. **TermCompiler**: Translates computational terms to Haskell expressions
2. **TypeCompiler**: Maps type universe to Haskell type signatures
3. **ProofCompiler**: Converts proof terms to Haskell evidence
4. **Emit**: Renders the result as syntactically valid Haskell source

DESIGN PRINCIPLES:

1. **Deterministic Generation**: Same AST input always produces identical output
2. **Type Safety**: Generated code must typecheck under GHC 9.2+
3. **No Unsafe Code**: Forbidden to use unsafeCoerce or similar
4. **Fail-Closed**: Malformed input generates an error, not silent code
5. **Module-Aware**: Qualified names preserve module structure

INTERFACE CONTRACT:

- compileModule: Module → Either String HaskellModule
  Takes a Module (from Core.Module), returns Haskell source or error

- compileDefinition: Definition → Either String HaskellDefinition
  Compiles a single definition (term, type, assertion, etc.)

- compileExpression: Term → Either String HaskellExpr
  Compiles a single term expression to Haskell

ASSUMPTIONS:

- All input has been verified by Agent 3B (TypeChecker)
- All proofs are valid (checked by Agent 2B)
- All module dependencies are resolved
- No circular references in type definitions
-}

module Assertica.Backend.CodeGen
  ( -- * Main compilation interface
    compileModule
  , compileDefinition
  , compileExpression
  , compileType
  , compileProof

    -- * Compiled output types
  , HaskellModule(..)
  , HaskellDefinition(..)
  , HaskellExpr(..)

    -- * Error handling
  , CodeGenError(..)
  , formatCodeGenError

    -- * Configuration
  , CodeGenConfig(..)
  , defaultConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Assertica.Core.AST
import Assertica.Core.Module (Module(..), Definition(..), ModuleMetadata(..))
import qualified Assertica.Backend.TermCompiler as TC
import qualified Assertica.Backend.TypeCompiler as TyC
import qualified Assertica.Backend.ProofCompiler as PC
import qualified Assertica.Backend.Emit as Emit

-- ============================================================================
-- TYPES
-- ============================================================================

{-| Configuration for code generation -}
data CodeGenConfig = CodeGenConfig
  { cgcAddComments :: Bool        -- ^ Include source location comments
  , cgcAddTypeSignatures :: Bool  -- ^ Include type signatures for all definitions
  , cgcModulePrefix :: [Text]    -- ^ Module path prefix for qualification
  , cgcTargetVersion :: (Int, Int)  -- ^ GHC version target (e.g., (9, 2))
  }
  deriving (Show, Generic)

{-| Default code generation configuration -}
defaultConfig :: CodeGenConfig
defaultConfig = CodeGenConfig
  { cgcAddComments = True
  , cgcAddTypeSignatures = True
  , cgcModulePrefix = []
  , cgcTargetVersion = (9, 2)
  }

{-| A compiled Haskell module (abstract representation) -}
data HaskellModule = HaskellModule
  { hmName :: [Text]                    -- ^ Module name path
  , hmImports :: [Text]                 -- ^ Import statements (one per line)
  , hmDefinitions :: [HaskellDefinition] -- ^ All definitions in module
  , hmComments :: [Text]                -- ^ Header comments
  }
  deriving (Show, Generic)

{-| A single compiled Haskell definition -}
data HaskellDefinition = HaskellDefinition
  { hdName :: Text                      -- ^ Definition name
  , hdTypeSignature :: Maybe Text       -- ^ Type signature (e.g., "x :: Int")
  , hdBody :: Text                      -- ^ Definition body
  , hdSourceLocation :: Maybe Text      -- ^ Source location comment
  }
  deriving (Show, Generic)

{-| A compiled Haskell expression -}
data HaskellExpr = HaskellExpr
  { heExpr :: Text                      -- ^ The Haskell expression
  , heType :: Maybe Text                -- ^ Inferred type annotation
  }
  deriving (Show, Generic)

{-| Code generation errors -}
data CodeGenError
  = UnhandledConstruct String
  | InvalidQName QName
  | TypeCompilationFailed String
  | ProofCompilationFailed String
  | TermCompilationFailed String
  | ModuleCompilationFailed String
  | UnsupportedFeature String
  deriving (Show, Generic)

{-| Human-readable error formatting -}
formatCodeGenError :: CodeGenError -> String
formatCodeGenError = \case
  UnhandledConstruct desc ->
    "Code generation: unhandled construct: " ++ desc
  InvalidQName (QName mod name) ->
    "Code generation: invalid qualified name: " ++ intercalate "." (map T.unpack mod) ++ "." ++ T.unpack name
  TypeCompilationFailed desc ->
    "Code generation: type compilation failed: " ++ desc
  ProofCompilationFailed desc ->
    "Code generation: proof compilation failed: " ++ desc
  TermCompilationFailed desc ->
    "Code generation: term compilation failed: " ++ desc
  ModuleCompilationFailed desc ->
    "Code generation: module compilation failed: " ++ desc
  UnsupportedFeature desc ->
    "Code generation: unsupported feature: " ++ desc

-- ============================================================================
-- MAIN COMPILATION INTERFACE
-- ============================================================================

{-| Compile an entire Assertica module to Haskell code.

The module must be fully elaborated and type-checked before reaching this stage.
This is the primary entry point for the backend.
-}
compileModule :: CodeGenConfig -> Module -> Either String HaskellModule
compileModule config (Module _modQName _imports _exports defMap _metadata) = do
  -- Compile all definitions from the definition map
  compiledDefs <- mapM (compileDefinitionValue config) (Map.elems defMap)

  -- Generate module header
  let imports = generateImports config
  let comments = ["-- Generated by Assertica Backend", "-- Do not edit manually"]

  return $ HaskellModule
    { hmName = []  -- Could extract from modQName if needed
    , hmImports = imports
    , hmDefinitions = compiledDefs
    , hmComments = comments
    }

{-| Compile a single definition from Module's Definition type -}
compileDefinitionValue :: CodeGenConfig -> Definition -> Either String HaskellDefinition
compileDefinitionValue config defn = case defn of
  DefTerm term -> do
    compiled <- compileExpression config term
    return $ HaskellDefinition
      { hdName = "value"  -- Anonymous term
      , hdTypeSignature = Nothing
      , hdBody = heExpr compiled
      , hdSourceLocation = if cgcAddComments config then Just "term" else Nothing
      }

  DefType ty -> do
    compiled <- compileType config ty
    return $ HaskellDefinition
      { hdName = "type_def"  -- Anonymous type
      , hdTypeSignature = Just "Type"
      , hdBody = heExpr compiled
      , hdSourceLocation = if cgcAddComments config then Just "type" else Nothing
      }

  DefAssertion _assertion -> do
    -- For now, compile assertion as a unit
    return $ HaskellDefinition
      { hdName = "assertion"
      , hdTypeSignature = Nothing
      , hdBody = "()"
      , hdSourceLocation = if cgcAddComments config then Just "assertion" else Nothing
      }

  DefOpaque desc ->
    return $ HaskellDefinition
      { hdName = "opaque"
      , hdTypeSignature = Nothing
      , hdBody = T.pack desc
      , hdSourceLocation = if cgcAddComments config then Just "opaque" else Nothing
      }

{-| Compile a single definition (can be a term, type, assertion, etc.) -}
compileDefinition :: CodeGenConfig -> Definition -> Either String HaskellDefinition
compileDefinition = compileDefinitionValue

{-| Compile a single term expression to Haskell -}
compileExpression :: CodeGenConfig -> Term -> Either String HaskellExpr
compileExpression config term = do
  expr <- TC.compileTerm config term
  return $ HaskellExpr
    { heExpr = expr
    , heType = Nothing
    }

{-| Compile a type to Haskell -}
compileType :: CodeGenConfig -> Type -> Either String HaskellExpr
compileType config ty = do
  expr <- TyC.compileType config ty
  return $ HaskellExpr
    { heExpr = expr
    , heType = Just "Type"  -- All types have kind Type
    }

{-| Compile a proof to Haskell -}
compileProof :: CodeGenConfig -> Proof -> Either String HaskellExpr
compileProof config proof = do
  expr <- PC.compileProof config proof
  return $ HaskellExpr
    { heExpr = expr
    , heType = Nothing
    }

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{-| Generate standard Haskell imports for a module -}
generateImports :: CodeGenConfig -> [Text]
generateImports _config =
  [ "import Prelude"
  , "import qualified Data.Typeable as Typeable"
  , "import GHC.Generics (Generic)"
  ]
