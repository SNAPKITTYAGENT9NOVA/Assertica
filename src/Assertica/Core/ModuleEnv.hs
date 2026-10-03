{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Assertica.Core.ModuleEnv
Description : Module environment and qualified name resolution
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The ModuleEnv manages loaded modules and handles name resolution:

1. **Module Loading**: Track loaded modules by qualified name
2. **Name Resolution**: Resolve qualified and unqualified names
3. **Import Resolution**: Handle qualified, unqualified, and selective imports
4. **Visibility Checking**: Enforce public/private boundaries
5. **Conflict Detection**: Error on name ambiguities
6. **Dependency Ordering**: Compute safe compilation order
-}

module Assertica.Core.ModuleEnv
  ( -- * Module environment
    ModuleEnv (..)
  , emptyModuleEnv
  , addModule

    -- * Name resolution
  , resolveName
  , resolveQName
  , resolveImports
  , resolveDependencies

    -- * Module lookup
  , lookupModule
  , findModule

    -- * Conflict detection
  , ConflictError (..)
  , detectNameConflicts
  , validateImports

    -- * Compilation ordering
  , CompilationOrder
  , computeCompilationOrder

    -- * Visibility
  , isNameVisible
  , checkNameAccess
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad (when)
import Data.Either (partitionEithers)

import Assertica.Core.AST (QName(..), Definition)
import Assertica.Core.Module
  ( Module(..), Import(..), Export(..), ExportItem(..)
  , Visibility(..), ModuleInterface(..), ImportStyle(..)
  , extractInterface, exportedNames, qualifyName
  )

-- ============================================================================
-- MODULE ENVIRONMENT
-- ============================================================================

{-| The module environment tracks all loaded modules and enables name resolution.
-}
data ModuleEnv = ModuleEnv
  { envModules :: Map QName Module             -- ^ All loaded modules
  , envInterfaces :: Map QName ModuleInterface -- ^ Module interfaces
  , envNameIndex :: Map Text [QName]           -- ^ Map from name to qualified names
  }
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Create an empty module environment.
-}
emptyModuleEnv :: ModuleEnv
emptyModuleEnv = ModuleEnv
  { envModules = Map.empty
  , envInterfaces = Map.empty
  , envNameIndex = Map.empty
  }

{-| Add a module to the environment.
Returns error if the module already exists or has circular dependencies.
-}
addModule :: ModuleEnv -> Module -> Either String ModuleEnv
addModule env m = do
  let modName = moduleName m
  when (Map.member modName (envModules env)) $
    Left $ "Module " ++ show modName ++ " already loaded"

  -- Check for circular dependencies
  let env' = env { envModules = Map.insert modName m (envModules env) }
  validateCircularDependencies env' modName

  -- Build interface and update index
  let iface = extractInterface m
  let env'' = env'
        { envInterfaces = Map.insert modName iface (envInterfaces env')
        }

  return env''

{-| Check if adding a module would create circular dependencies.
-}
validateCircularDependencies :: ModuleEnv -> QName -> Either String ()
validateCircularDependencies env modName =
  case detectCircularChain env modName of
    [] -> Right ()
    chain -> Left $ "Circular dependency: " ++ show chain

{-| Detect if a module creates a circular dependency chain.
-}
detectCircularChain :: ModuleEnv -> QName -> [QName]
detectCircularChain env start =
  detectChain env start start []
  where
    detectChain _ _ _ path | length path > 100 = []  -- Prevent infinite loops
    detectChain env current target path
      | current /= start && current == target = current : path
      | otherwise =
        case Map.lookup current (envModules env) of
          Nothing -> []
          Just m ->
            let deps = [importModule imp | imp <- moduleImports m]
            in concatMap (\d -> detectChain env d target (current : path)) deps

-- ============================================================================
-- NAME RESOLUTION
-- ============================================================================

{-| Resolve an unqualified name in a module's context.
Returns the fully qualified name and definition.
-}
resolveName :: ModuleEnv -> Module -> Text -> Either String (QName, Definition)
resolveName env currentMod name = do
  -- First check local definitions
  case Map.lookup name (moduleDefinitions currentMod) of
    Just def -> Right (qualifyName currentMod name, def)
    Nothing -> do
      -- Then check imports
      candidates <- resolveNameViaImports env currentMod name
      case candidates of
        [] -> Left $ "Name '" ++ T.unpack name ++ "' not found"
        [(qn, def)] -> Right (qn, def)
        multiple -> Left $ "Ambiguous name '" ++ T.unpack name ++ "': " ++
                          show (map fst multiple)

{-| Resolve a qualified name.
-}
resolveQName :: ModuleEnv -> QName -> Either String Definition
resolveQName env qname@(QName mod _) = do
  -- Look up the module
  let modQName = if null mod then error "Empty module path" else QName (init mod) (head (reverse mod))
  m <- case Map.lookup modQName (envModules env) of
    Just m -> Right m
    Nothing -> Left $ "Module " ++ show modQName ++ " not found"

  -- Look up the definition in the module
  case Map.lookup (last (qnameModule qname ++ [qnameLocal qname])) (moduleDefinitions m) of
    Just def -> Right def
    Nothing -> Left $ "Definition " ++ show qname ++ " not found in module"

{-| Find all possible qualified names for an unqualified name through imports.
-}
resolveNameViaImports :: ModuleEnv -> Module -> Text -> Either String [(QName, Definition)]
resolveNameViaImports env currentMod name = do
  let imports = moduleImports currentMod

  candidates <- concat <$> mapM (resolveImport env currentMod name) imports

  case candidates of
    [] -> Left $ "Name '" ++ T.unpack name ++ "' not imported"
    lst -> Right lst

{-| Resolve a name through a specific import statement.
-}
resolveImport :: ModuleEnv -> Module -> Text -> Import -> Either String [(QName, Definition)]
resolveImport env _ name imp = do
  let importedMod = importModule imp
  m <- case Map.lookup importedMod (envModules env) of
    Just m -> Right m
    Nothing -> Left $ "Module " ++ show importedMod ++ " not found"

  -- Check if the name is exported
  unless (Set.member name (exportedNames m))
    (Left $ "Name '" ++ T.unpack name ++ "' not exported from " ++ show importedMod)

  -- Look up in the module
  case Map.lookup name (moduleDefinitions m) of
    Just def -> do
      let qn = QName (qnameModule importedMod) name
      Right [(qn, def)]
    Nothing -> Left $ "Definition '" ++ T.unpack name ++ "' not found"
  where
    unless cond err = if cond then Right () else Left err

{-| Resolve all imports in a module.
Returns a map from import alias to module.
-}
resolveImports :: ModuleEnv -> Module -> Either String (Map QName Module)
resolveImports env m = do
  pairs <- mapM (resolveImportToModule env) (moduleImports m)
  return $ Map.fromList pairs
  where
    resolveImportToModule env imp = do
      let impMod = importModule imp
      m <- case Map.lookup impMod (envModules env) of
        Just m -> Right m
        Nothing -> Left $ "Cannot resolve import: " ++ show impMod
      let alias = case importAlias imp of
            Just a -> a
            Nothing -> impMod
      return (alias, m)

{-| Resolve all dependencies of a module.
Returns modules sorted in dependency order.
-}
resolveDependencies :: ModuleEnv -> QName -> Either String [Module]
resolveDependencies env modName = do
  m <- case Map.lookup modName (envModules env) of
    Just m -> Right m
    Nothing -> Left $ "Module not found: " ++ show modName

  -- Collect all direct dependencies
  let depNames = [importModule imp | imp <- moduleImports m]
  deps <- mapM (\name -> case Map.lookup name (envModules env) of
                           Just m -> Right m
                           Nothing -> Left $ "Dependency not found: " ++ show name)
               depNames

  return (m : deps)

-- ============================================================================
-- MODULE LOOKUP
-- ============================================================================

{-| Look up a module by name.
-}
lookupModule :: ModuleEnv -> QName -> Maybe Module
lookupModule env name = Map.lookup name (envModules env)

{-| Find a module by partial name.
Returns all modules whose name ends with the given suffix.
-}
findModule :: ModuleEnv -> [Text] -> [Module]
findModule env suffix =
  filter matches (Map.elems (envModules env))
  where
    matches m =
      let (QName mod _) = moduleName m
      in take (length suffix) (reverse mod) == reverse suffix

-- ============================================================================
-- CONFLICT DETECTION
-- ============================================================================

{-| An error from conflict detection.
-}
data ConflictError
  = DuplicateExport Text QName QName      -- ^ Same name exported from two modules
  | DuplicateImport Text QName QName      -- ^ Same name imported twice
  | PrivateAccessDenied Text QName        -- ^ Attempt to access private name
  | CircularDependency [QName]            -- ^ Circular import chain
  | UnresolvedImport QName                -- ^ Import cannot be resolved
  deriving (Eq, Show, Generic, Typeable, Data)

{-| Detect name conflicts in imports.
Returns a list of conflicts.
-}
detectNameConflicts :: ModuleEnv -> Module -> [ConflictError]
detectNameConflicts env m =
  detectDuplicateImports m ++
  detectPrivateAccess env m

{-| Detect duplicate imports.
-}
detectDuplicateImports :: Module -> [ConflictError]
detectDuplicateImports m =
  let imports = moduleImports m
      importedNames = concatMap extractImportedNames imports
      grouped = group (sort importedNames)
      duplicates = [name | group <- grouped, length group > 1, let name = head group]
  in map (\name -> DuplicateImport (fst name) (snd name) (snd (head (filter (\n -> fst n == name) importedNames))))
         (nub duplicates)
  where
    nub = map head . group . sort
    extractImportedNames imp = case importStyle imp of
      Selective names -> [(name, importModule imp) | name <- names]
      _ -> []

{-| Detect attempts to access private definitions.
-}
detectPrivateAccess :: ModuleEnv -> Module -> [ConflictError]
detectPrivateAccess env m =
  let imports = moduleImports m
  in concatMap checkImportAccess imports
  where
    checkImportAccess imp =
      case Map.lookup (importModule imp) (envModules env) of
        Nothing -> [UnresolvedImport (importModule imp)]
        Just impMod ->
          let exported = exportItems (moduleExports impMod)
              privateNames = [name | ExportDef name Private <- exported]
          in case importStyle imp of
            Selective names ->
              [PrivateAccessDenied name (importModule imp) | name <- names, elem name privateNames]
            _ -> []

{-| Validate all imports in a module.
Returns list of validation errors.
-}
validateImports :: ModuleEnv -> Module -> Either String ()
validateImports env m = do
  let errors = detectNameConflicts env m
  if null errors
    then Right ()
    else Left $ "Import validation failed: " ++ show errors

-- ============================================================================
-- COMPILATION ORDERING
-- ============================================================================

{-| Compilation order: modules sorted such that dependencies come first.
-}
type CompilationOrder = [QName]

{-| Compute compilation order using topological sort.
Returns modules in order (dependencies first).
-}
computeCompilationOrder :: ModuleEnv -> Either String CompilationOrder
computeCompilationOrder env = do
  let mods = Map.keys (envModules env)
  topologicalSort env mods
  where
    topologicalSort env modNames
      | null modNames = Right []
      | otherwise =
          case findModulesWithoutDeps env modNames of
            [] -> Left "Circular dependencies detected"
            roots -> do
              let remaining = filter (`notElem` roots) modNames
              rest <- topologicalSort env remaining
              Right (roots ++ rest)

    findModulesWithoutDeps env modNames =
      filter (noDeps env modNames) modNames

    noDeps env allMods modName =
      case Map.lookup modName (envModules env) of
        Nothing -> False
        Just m ->
          let deps = [importModule imp | imp <- moduleImports m]
          in all (`notElem` allMods) deps

-- ============================================================================
-- VISIBILITY CHECKING
-- ============================================================================

{-| Check if a name is visible from outside its module.
-}
isNameVisible :: Module -> Text -> Bool
isNameVisible m name =
  Set.member name (exportedNames m)

{-| Check if a name can be accessed by another module.
-}
checkNameAccess :: ModuleEnv -> QName -> QName -> Either String ()
checkNameAccess env fromMod toMod@(QName _ name) = do
  targetMod <- case Map.lookup toMod (envModules env) of
    Just m -> Right m
    Nothing -> Left $ "Module not found: " ++ show toMod

  unless (isNameVisible targetMod name)
    (Left $ "Name " ++ show toMod ++ " is not exported")
  where
    unless cond err = if cond then Right () else Left err
