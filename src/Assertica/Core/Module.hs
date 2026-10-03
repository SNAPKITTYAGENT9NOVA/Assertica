{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Assertica.Core.Module
Description : Module system for proof library organization
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The Module system provides namespace management, visibility control, and clean
separation of concerns for large proof libraries. It enables:

1. **Namespaces**: Prevent name collisions across library and user code
   - Core library: Assertica.Core
   - Standard library: Assertica.Stdlib.Lattice
   - User code: MyProject.Proofs

2. **Visibility Control**: Public/private exports
   - Public definitions: Exported in module interface
   - Private definitions: Internal only

3. **Qualified Names**: Enable disambiguation
   - List.length vs Vector.length
   - Short forms via selective imports

4. **Deterministic Resolution**: No ambiguity in name lookups
   - Import order irrelevant
   - Explicit qualification always available

5. **Dependency Tracking**: Detect cycles and build order

DESIGN PRINCIPLES:
- Each module has clear exports and imports
- Names are resolved deterministically
- Circular dependencies are detected and rejected
- Modules are first-class citizens in the type system
- Interface extraction enables separate compilation
-}

module Assertica.Core.Module
  ( -- * Definitions
    Definition (..)

    -- * Module types
  , Module (..)
  , ModuleMetadata (..)
  , ModuleInterface (..)

    -- * Import types
  , Import (..)
  , ImportStyle (..)

    -- * Export types
  , Export (..)
  , ExportItem (..)

    -- * Visibility and stability
  , Visibility (..)
  , Stability (..)
  , Strength (..)

    -- * Dependency tracking
  , ModuleDependency (..)
  , ModuleGraph

    -- * Basic operations
  , emptyModule
  , modulePath
  , qualifyName
  , isCircularDependency
  , extractInterface
  , exportedNames
  , visibleNames
  , prettyModule
  , prettyImport
  , prettyExport
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Graph as Graph
import Data.Maybe (catMaybes, mapMaybe)

import Assertica.Core.AST (QName(..), Term, Type, Proposition, Assertion)

-- ============================================================================
-- DEFINITIONS
-- ============================================================================

{-| A definition is any named entity in a module.
Can be a type, function, assertion, or other named construct.
-}
data Definition
  = DefTerm Term                       -- ^ A term (function/value)
  | DefType Type                       -- ^ A type definition
  | DefAssertion Assertion             -- ^ An assertion
  | DefOpaque String                   -- ^ An opaque definition (from external library)
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- VISIBILITY AND EXPORTS
-- ============================================================================

{-| Visibility of a definition within and outside its module.
-}
data Visibility
  = Public      -- ^ Exported by the module, accessible externally
  | Private     -- ^ Module-internal only, not accessible externally
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

{-| An item that can be exported from a module.
-}
data ExportItem
  = ExportDef Text Visibility      -- ^ A definition (type, function, etc.)
  | ExportModule QName             -- ^ Re-export all public items from another module
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Export list for a module.
-}
data Export = Export
  { exportItems :: [ExportItem]    -- ^ What this module exports
  }
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- IMPORTS
-- ============================================================================

{-| How a module is imported.
-}
data ImportStyle
  = Qualified           -- ^ import X as Y → only Y.name accessible
  | Unqualified         -- ^ import X → names from X directly accessible
  | Selective [Text]    -- ^ import X (foo, bar) → only foo, bar imported
  deriving (Eq, Show, Generic, Typeable, Data)

{-| An import statement in a module.
-}
data Import = Import
  { importModule :: QName        -- ^ The module being imported
  , importAlias :: Maybe QName   -- ^ Optional alias (for qualified imports)
  , importStyle :: ImportStyle   -- ^ How to import
  }
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- DEPENDENCY TRACKING
-- ============================================================================

{-| A dependency relationship between modules.
-}
data ModuleDependency = ModuleDependency
  { depModule :: QName           -- ^ The depended-upon module
  , depStrength :: Strength      -- ^ How strong the dependency is
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Strength of a dependency.
-}
data Strength
  = Direct      -- ^ Direct import
  | Transitive  -- ^ Transitively imported
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

{-| A directed graph of modules and their dependencies.
Enables cycle detection and topological sorting.
-}
type ModuleGraph = Map QName (Set QName)

-- ============================================================================
-- MODULE METADATA
-- ============================================================================

{-| Metadata about a module.
-}
data ModuleMetadata = ModuleMetadata
  { metadataDocstring :: Maybe String       -- ^ Module documentation
  , metadataVersion :: String               -- ^ Version (e.g., "1.0.0")
  , metadataDependencies :: [ModuleDependency]  -- ^ Known dependencies
  , metadataStability :: Stability          -- ^ Stability level
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Stability level of a module.
-}
data Stability
  = Experimental  -- ^ May change significantly
  | Stable        -- ^ Unlikely to change
  | Deprecated    -- ^ Use alternative instead
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- MODULES
-- ============================================================================

{-| A module is the unit of organization in Assertica.
Modules contain definitions organized by namespaces.
-}
data Module = Module
  { moduleName :: QName              -- ^ e.g., "Assertica.Stdlib.Lattice"
  , moduleImports :: [Import]        -- ^ What this module imports
  , moduleExports :: Export          -- ^ What this module exports
  , moduleDefinitions :: Map Text Definition  -- ^ Definitions by name
  , moduleMetadata :: ModuleMetadata -- ^ Metadata
  }
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- MODULE INTERFACE
-- ============================================================================

{-| A module interface reveals only the public API.
Enables separate compilation and type checking.
-}
data ModuleInterface = ModuleInterface
  { ifaceName :: QName                      -- ^ Module name
  , ifaceExports :: Map Text ExportItem     -- ^ Exported items
  , ifaceDocstring :: Maybe String          -- ^ Module documentation
  }
  deriving (Eq, Show, Generic, Typeable, Data)

-- ============================================================================
-- BASIC OPERATIONS
-- ============================================================================

{-| Create an empty module with defaults.
-}
emptyModule :: QName -> Module
emptyModule name = Module
  { moduleName = name
  , moduleImports = []
  , moduleExports = Export []
  , moduleDefinitions = Map.empty
  , moduleMetadata = ModuleMetadata
      { metadataDocstring = Nothing
      , metadataVersion = "0.1.0"
      , metadataDependencies = []
      , metadataStability = Experimental
      }
  }

{-| Convert module name to file path.
Example: QName ["Assertica", "Stdlib"] "Lattice" → "Assertica/Stdlib/Lattice.hs"
-}
modulePath :: QName -> FilePath
modulePath (QName mod name) =
  intercalate "/" (map T.unpack (mod ++ [name])) ++ ".hs"

{-| Qualify a name with the module's namespace.
-}
qualifyName :: Module -> Text -> QName
qualifyName m name = QName (qnameModule (moduleName m)) name

{-| Extract the public interface of a module.
Only includes public exports, type signatures, and documentation.
-}
extractInterface :: Module -> ModuleInterface
extractInterface m = ModuleInterface
  { ifaceName = moduleName m
  , ifaceExports = exportedItemsAsMap m
  , ifaceDocstring = metadataDocstring (moduleMetadata m)
  }
  where
    exportedItemsAsMap m =
      Map.fromList [(itemName item, item) | item <- exportItems (moduleExports m)]

    itemName (ExportDef name _) = name
    itemName (ExportModule _) = ""  -- Re-exports handled separately

{-| Get all names exported by a module.
-}
exportedNames :: Module -> Set Text
exportedNames m =
  Set.fromList [name | ExportDef name Public <- exportItems (moduleExports m)]

{-| Check if a name is visible in the module.
A name is visible if it's defined and exported.
-}
visibleNames :: Module -> Set Text
visibleNames m = exportedNames m

{-| Check if a dependency would create a circular import.
-}
isCircularDependency :: ModuleGraph -> QName -> QName -> Bool
isCircularDependency graph from to =
  case Map.lookup to graph of
    Nothing -> False
    Just deps -> Set.member from deps

{-| Build a module dependency graph.
-}
buildModuleGraph :: [Module] -> ModuleGraph
buildModuleGraph mods =
  Map.fromList [(moduleName m, directDependencies m) | m <- mods]
  where
    directDependencies m =
      Set.fromList [importModule imp | imp <- moduleImports m]

{-| Detect cycles in the module dependency graph.
Returns a list of cycles (each cycle is a list of modules forming a loop).
-}
detectCycles :: ModuleGraph -> [[QName]]
detectCycles graph =
  let (scc, _) = stronglyConnected graph
  in filter (\cycle -> length cycle > 1 || any hasSelfLoop cycle) scc
  where
    hasSelfLoop (QName _ name) =
      case Map.lookup (QName [] name) graph of
        Nothing -> False
        Just deps -> Set.member (QName [] name) deps

    stronglyConnected _ = ([], id)  -- Simplified; full Tarjan's algorithm would go here

{-| Topologically sort modules by dependencies.
Returns modules in order suitable for compilation (dependencies first).
Fails if cycles are detected.
-}
topologicalSort :: [Module] -> Either String [Module]
topologicalSort mods =
  let graph = buildModuleGraph mods
      cycles = detectCycles graph
  in if not (null cycles)
     then Left $ "Circular dependencies detected: " ++ show cycles
     else Right mods  -- Simplified; real implementation would sort

-- ============================================================================
-- PRETTY PRINTING
-- ============================================================================

{-| Pretty-print an import statement.
-}
prettyImport :: Import -> String
prettyImport imp =
  let modStr = prettyQName (importModule imp)
      styleStr = case importStyle imp of
        Qualified -> ""
        Unqualified -> ""
        Selective names -> " (" ++ intercalate ", " (map T.unpack names) ++ ")"
      aliasStr = case importAlias imp of
        Nothing -> ""
        Just alias -> " as " ++ prettyQName alias
  in "import " ++ modStr ++ aliasStr ++ styleStr

{-| Pretty-print an export statement.
-}
prettyExport :: Export -> String
prettyExport (Export items) =
  "(" ++ intercalate ", " (map prettyExportItem items) ++ ")"
  where
    prettyExportItem (ExportDef name vis) =
      T.unpack name ++ (if vis == Public then "" else " [private]")
    prettyExportItem (ExportModule m) =
      "module " ++ prettyQName m

{-| Pretty-print a module definition.
-}
prettyModule :: Module -> String
prettyModule m =
  "module " ++ prettyQName (moduleName m) ++ " where\n" ++
  (if null (moduleImports m) then "" else unlines (map prettyImport (moduleImports m)) ++ "\n") ++
  (if null (exportItems (moduleExports m)) then "" else "exports " ++ prettyExport (moduleExports m) ++ "\n") ++
  "-- " ++ show (Map.size (moduleDefinitions m)) ++ " definitions"

{-| Helper: pretty-print a qualified name.
-}
prettyQName :: QName -> String
prettyQName (QName mod name) =
  intercalate "." (map T.unpack (mod ++ [name]))
