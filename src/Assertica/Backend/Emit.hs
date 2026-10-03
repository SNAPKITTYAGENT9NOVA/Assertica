{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Assertica.Backend.Emit
Description : Emit syntactically valid Haskell source code
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Converts compiled intermediate representations (from CodeGen, TermCompiler, etc.)
into syntactically valid Haskell 2010 source code that can be processed by GHC.

RESPONSIBILITIES:

1. **Module Headers**: Generate correct module declaration with exports
2. **Imports**: Produce valid import statements with proper syntax
3. **Definitions**: Format term and type definitions with correct syntax
4. **Type Signatures**: Generate type declarations with proper indentation
5. **Comments**: Add source location hints and documentation
6. **Pretty-Printing**: Consistent, readable formatting

HASKELL SYNTAX REQUIREMENTS:

- Module header: "module Name (exports) where"
- Imports: "import [qualified] Module [(selective)]"
- Type signature: "name :: type"
- Definition: "name = body" or "name pattern = body"
- Indentation: 2 spaces (configurable)

PRINCIPLES:

1. **Determinism**: Same input produces identical Haskell source
2. **Validity**: All generated code must parse as Haskell 2010 + GHC extensions
3. **Readability**: Human-readable formatting for debugging
4. **Completeness**: All AST nodes properly formatted
-}

module Assertica.Backend.Emit
  ( -- * Main emission functions
    emitHaskellModule
  , emitHaskellDefinition
  , emitHaskellExpression

    -- * Formatting utilities
  , prettyPrint
  , formatModuleHeader
  , formatImports
  , formatDefinition
  , formatTypeSignature

    -- * Configuration
  , EmitConfig(..)
  , defaultEmitConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import GHC.Generics (Generic)

import Assertica.Backend.CodeGen
  ( HaskellModule(..)
  , HaskellDefinition(..)
  , HaskellExpr(..)
  )

-- ============================================================================
-- EMIT CONFIGURATION
-- ============================================================================

{-| Configuration for Haskell code emission -}
data EmitConfig = EmitConfig
  { ecIndentSize :: Int           -- ^ Spaces per indentation level (default: 2)
  , ecLineLength :: Int           -- ^ Target line length (default: 80)
  , ecAddComments :: Bool         -- ^ Include source comments (default: True)
  , ecPreludeImports :: Bool      -- ^ Auto-import Prelude (default: True)
  }
  deriving (Show, Generic)

{-| Default emission configuration -}
defaultEmitConfig :: EmitConfig
defaultEmitConfig = EmitConfig
  { ecIndentSize = 2
  , ecLineLength = 80
  , ecAddComments = True
  , ecPreludeImports = True
  }

-- ============================================================================
-- MAIN EMISSION FUNCTIONS
-- ============================================================================

{-| Emit a complete Haskell module as source code.

This is the primary interface for generating output .hs files.
Takes a HaskellModule and produces a complete, syntactically valid Haskell
program that can be compiled with GHC.
-}
emitHaskellModule :: EmitConfig -> HaskellModule -> Text
emitHaskellModule config (HaskellModule modName imports defs comments) =
  T.unlines $
    -- Comments
    map (T.cons '-') comments ++
    [""]

    -- Module header
    ++ [formatModuleHeader modName]

    -- Imports
    ++ (if null imports then [] else [""] ++ formatImports config imports)

    -- Definitions
    ++ (if null defs then [] else [""] ++ map (emitHaskellDefinition config) defs)

{-| Emit a single Haskell definition -}
emitHaskellDefinition :: EmitConfig -> HaskellDefinition -> Text
emitHaskellDefinition config (HaskellDefinition name typeSig body sourceLocation) =
  let
    -- Type signature (if present)
    sigLines = case typeSig of
      Nothing -> []
      Just sig -> [formatTypeSignature name sig]

    -- Definition line
    defLine = formatDefinition name body

    -- Source location comment
    commentLines = case sourceLocation of
      Nothing -> []
      Just loc | ecAddComments config -> ["-- Source: " <> loc]
      _ -> []
  in
    T.unlines $ commentLines ++ sigLines ++ [defLine]

{-| Emit a single Haskell expression -}
emitHaskellExpression :: EmitConfig -> HaskellExpr -> Text
emitHaskellExpression _config (HaskellExpr expr _mtype) =
  expr

-- ============================================================================
-- FORMATTING FUNCTIONS
-- ============================================================================

{-| Format module header -}
formatModuleHeader :: [Text] -> Text
formatModuleHeader modName =
  "module " <> T.intercalate "." modName <> " where"

{-| Format import statements -}
formatImports :: EmitConfig -> [Text] -> [Text]
formatImports _config imports =
  map ("import " <>) imports

{-| Format a type signature -}
formatTypeSignature :: Text -> Text -> Text
formatTypeSignature name typeSig =
  name <> " :: " <> typeSig

{-| Format a definition with name and body -}
formatDefinition :: Text -> Text -> Text
formatDefinition name body =
  name <> " = " <> body

-- ============================================================================
-- PRETTY-PRINTING
-- ============================================================================

{-| Pretty-print a piece of code with proper indentation and formatting.

This takes raw code (possibly from a compiler) and formats it nicely
for human consumption and debugging.
-}
prettyPrint :: EmitConfig -> Text -> Text
prettyPrint config code =
  -- Add proper indentation, line wrapping, etc.
  -- For now, return as-is
  code

{-| Indent a text block by n levels -}
indent :: Int -> Text -> Text
indent n txt =
  let spaces = T.replicate n " "
      lines_ = T.lines txt
  in T.unlines $ map (spaces <>) lines_

{-| Wrap text to specified line length -}
wrap :: Int -> Text -> Text
wrap _lineLen txt =
  -- Simple implementation: just return as-is
  -- A full implementation would break at spaces
  txt
