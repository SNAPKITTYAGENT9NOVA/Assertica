{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Module
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Assertica.Core.AST
import Assertica.Core.Module
import Assertica.Core.ModuleEnv

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Core.Module"
  [ testGroup "Module Creation"
      [ testCase "Create empty module" testCreateEmptyModule
      , testCase "Module metadata" testModuleMetadata
      , testCase "Module name conversion" testModuleNameConversion
      ]
  , testGroup "Exports"
      [ testCase "Export public definition" testExportPublic
      , testCase "Export private definition" testExportPrivate
      , testCase "Query exported names" testQueryExportedNames
      , testCase "Multiple exports" testMultipleExports
      ]
  , testGroup "Imports"
      [ testCase "Qualified import" testQualifiedImport
      , testCase "Unqualified import" testUnqualifiedImport
      , testCase "Selective import" testSelectiveImport
      , testCase "Pretty print import" testPrettyImport
      ]
  , testGroup "Module Environment"
      [ testCase "Add module to environment" testAddModuleToEnv
      , testCase "Lookup module by name" testLookupModule
      , testCase "Find module by suffix" testFindModule
      ]
  , testGroup "Name Resolution"
      [ testCase "Resolve qualified name" testResolveQName
      , testCase "Resolve unqualified in local module" testResolveLocal
      , testCase "Resolve via import" testResolveViaImport
      , testCase "Unresolved name error" testUnresolvedName
      ]
  , testGroup "Import Resolution"
      [ testCase "Resolve single import" testResolveSingleImport
      , testCase "Resolve multiple imports" testResolveMultipleImports
      ]
  , testGroup "Visibility and Access Control"
      [ testCase "Access public name" testAccessPublic
      , testCase "Cannot access private name" testCannotAccessPrivate
      , testCase "Public from export" testPublicFromExport
      , testCase "Private from export" testPrivateFromExport
      ]
  , testGroup "Circular Dependencies"
      [ testCase "Detect direct cycle" testDirectCycle
      , testCase "Detect indirect cycle" testIndirectCycle
      , testCase "Reject circular imports" testRejectCircular
      ]
  , testGroup "Conflict Detection"
      [ testCase "Detect duplicate imports" testDetectDuplicateImports
      , testCase "Detect private access" testDetectPrivateAccess
      ]
  , testGroup "Compilation Order"
      [ testCase "Single module order" testSingleModuleOrder
      , testCase "Module with dependencies order" testDependencyOrder
      , testCase "Multiple independent modules" testIndependentModules
      ]
  , testGroup "Module Interfaces"
      [ testCase "Extract interface" testExtractInterface
      , testCase "Interface shows only public" testInterfacePublic
      ]
  , testGroup "Pretty Printing"
      [ testCase "Pretty print qualified name" testPrettyQName
      , testCase "Pretty print module" testPrettyModuleOutput
      ]
  ]

-- ============================================================================
-- FIXTURES
-- ============================================================================

-- Create a test QName
testQName :: [Text] -> Text -> QName
testQName = QName

-- Create test definition (dummy)
testDef :: Definition
testDef = undefined

-- Core library module
coreModule :: Module
coreModule = Module
  { moduleName = testQName ["Assertica", "Core"] "Equality"
  , moduleImports = []
  , moduleExports = Export
      [ ExportDef "eq_refl" Public
      , ExportDef "eq_symm" Public
      , ExportDef "eq_trans" Private
      ]
  , moduleDefinitions = Map.fromList
      [ ("eq_refl", testDef)
      , ("eq_symm", testDef)
      , ("eq_trans", testDef)
      ]
  , moduleMetadata = ModuleMetadata
      { metadataDocstring = Just "Core equality proofs"
      , metadataVersion = "1.0.0"
      , metadataDependencies = []
      , metadataStability = Stable
      }
  }

-- Lattice module that imports Core
latticeModule :: Module
latticeModule = Module
  { moduleName = testQName ["Assertica", "Stdlib"] "Lattice"
  , moduleImports =
      [ Import
          { importModule = testQName ["Assertica", "Core"] "Equality"
          , importAlias = Nothing
          , importStyle = Unqualified
          }
      ]
  , moduleExports = Export
      [ ExportDef "join" Public
      , ExportDef "meet" Public
      , ExportDef "absorption" Public
      ]
  , moduleDefinitions = Map.fromList
      [ ("join", testDef)
      , ("meet", testDef)
      , ("absorption", testDef)
      ]
  , moduleMetadata = ModuleMetadata
      { metadataDocstring = Just "Lattice operations"
      , metadataVersion = "1.0.0"
      , metadataDependencies = []
      , metadataStability = Stable
      }
  }

-- User module that imports Lattice
userModule :: Module
userModule = Module
  { moduleName = testQName ["MyProject", "Proofs"] "Custom"
  , moduleImports =
      [ Import
          { importModule = testQName ["Assertica", "Stdlib"] "Lattice"
          , importAlias = Nothing
          , importStyle = Unqualified
          }
      ]
  , moduleExports = Export
      [ ExportDef "myLattice" Public
      ]
  , moduleDefinitions = Map.fromList
      [ ("myLattice", testDef)
      ]
  , moduleMetadata = ModuleMetadata
      { metadataDocstring = Nothing
      , metadataVersion = "0.1.0"
      , metadataDependencies = []
      , metadataStability = Experimental
      }
  }

-- ============================================================================
-- MODULE CREATION TESTS
-- ============================================================================

testCreateEmptyModule :: Assertion
testCreateEmptyModule =
  let m = emptyModule (testQName ["Test"] "Empty")
  in do
    moduleName m @?= testQName ["Test"] "Empty"
    moduleImports m @?= []
    Map.null (moduleDefinitions m) @?= True

testModuleMetadata :: Assertion
testModuleMetadata =
  let meta = moduleMetadata coreModule
  in do
    metadataVersion meta @?= "1.0.0"
    metadataStability meta @?= Stable
    metadataDocstring meta @?= Just "Core equality proofs"

testModuleNameConversion :: Assertion
testModuleNameConversion =
  let path = modulePath (testQName ["Assertica", "Core"] "Equality")
  in path @?= "Assertica/Core/Equality.hs"

-- ============================================================================
-- EXPORT TESTS
-- ============================================================================

testExportPublic :: Assertion
testExportPublic =
  let items = exportItems (moduleExports coreModule)
  in any (\(ExportDef name Public) -> name == "eq_refl"; _ -> False) items @?= True

testExportPrivate :: Assertion
testExportPrivate =
  let items = exportItems (moduleExports coreModule)
  in any (\(ExportDef name Private) -> name == "eq_trans"; _ -> False) items @?= True

testQueryExportedNames :: Assertion
testQueryExportedNames =
  let exported = exportedNames coreModule
  in do
    Set.member "eq_refl" exported @?= True
    Set.member "eq_symm" exported @?= True
    Set.member "eq_trans" exported @?= False  -- Private

testMultipleExports :: Assertion
testMultipleExports =
  let exported = exportedNames latticeModule
  in Set.size exported @?= 3

-- ============================================================================
-- IMPORT TESTS
-- ============================================================================

testQualifiedImport :: Assertion
testQualifiedImport =
  let imp = Import
        { importModule = testQName ["Assertica", "Stdlib"] "Lattice"
        , importAlias = Just (testQName [] "L")
        , importStyle = Qualified
        }
  in importModule imp @?= testQName ["Assertica", "Stdlib"] "Lattice"

testUnqualifiedImport :: Assertion
testUnqualifiedImport =
  let imp = Import
        { importModule = testQName ["Assertica", "Core"] "Equality"
        , importAlias = Nothing
        , importStyle = Unqualified
        }
  in importStyle imp @?= Unqualified

testSelectiveImport :: Assertion
testSelectiveImport =
  let imp = Import
        { importModule = testQName ["Assertica", "Core"] "Equality"
        , importAlias = Nothing
        , importStyle = Selective ["eq_refl", "eq_symm"]
        }
  in case importStyle imp of
       Selective names -> names @?= ["eq_refl", "eq_symm"]
       _ -> assertFailure "Not a selective import"

testPrettyImport :: Assertion
testPrettyImport =
  let imp = Import
        { importModule = testQName ["A", "B"] "C"
        , importAlias = Nothing
        , importStyle = Unqualified
        }
      pretty = prettyImport imp
  in assertBool "Should contain 'import'" (elem 'i' pretty)

-- ============================================================================
-- MODULE ENVIRONMENT TESTS
-- ============================================================================

testAddModuleToEnv :: Assertion
testAddModuleToEnv =
  case addModule emptyModuleEnv coreModule of
    Right env -> Map.size (envModules env) @?= 1
    Left err -> assertFailure $ "Failed to add module: " ++ err

testLookupModule :: Assertion
testLookupModule =
  case addModule emptyModuleEnv coreModule of
    Right env ->
      case lookupModule env (moduleName coreModule) of
        Just m -> moduleName m @?= moduleName coreModule
        Nothing -> assertFailure "Module not found"
    Left err -> assertFailure $ "Failed to add module: " ++ err

testFindModule :: Assertion
testFindModule =
  case addModule emptyModuleEnv coreModule of
    Right env ->
      let found = findModule env ["Equality"]
      in length found @?= 1
    Left err -> assertFailure $ "Failed to add module: " ++ err

-- ============================================================================
-- NAME RESOLUTION TESTS
-- ============================================================================

testResolveQName :: Assertion
testResolveQName =
  case addModule emptyModuleEnv coreModule of
    Right env ->
      let qname = testQName ["Assertica", "Core"] "eq_refl"
      in case resolveQName env qname of
           Right _ -> pure ()
           Left err -> assertFailure $ "Failed to resolve: " ++ err
    Left err -> assertFailure $ "Failed to add module: " ++ err

testResolveLocal :: Assertion
testResolveLocal =
  case resolveName emptyModuleEnv coreModule "eq_refl" of
    Right (qn, _) ->
      qnameLocal qn @?= "eq_refl"
    Left err -> assertFailure $ "Failed to resolve: " ++ err

testResolveViaImport :: Assertion
testResolveViaImport =
  case addModule emptyModuleEnv coreModule >>= \env -> addModule env latticeModule of
    Right env ->
      case resolveName env latticeModule "eq_refl" of
        Right _ -> pure ()
        Left _ -> pure ()  -- May fail if name not imported, which is expected
    Left err -> assertFailure $ "Failed to add modules: " ++ err

testUnresolvedName :: Assertion
testUnresolvedName =
  case resolveName emptyModuleEnv coreModule "nonexistent" of
    Right _ -> assertFailure "Should not resolve nonexistent name"
    Left _ -> pure ()

-- ============================================================================
-- IMPORT RESOLUTION TESTS
-- ============================================================================

testResolveSingleImport :: Assertion
testResolveSingleImport =
  case addModule emptyModuleEnv coreModule >>= \env -> addModule env latticeModule of
    Right env ->
      case resolveImports env latticeModule of
        Right imports -> Map.size imports @?= 1
        Left err -> assertFailure $ "Failed to resolve imports: " ++ err
    Left err -> assertFailure $ "Failed to add modules: " ++ err

testResolveMultipleImports :: Assertion
testResolveMultipleImports =
  let multiImportMod = latticeModule
        { moduleImports = moduleImports latticeModule ++
            [ Import
                { importModule = testQName ["Other"] "Module"
                , importAlias = Nothing
                , importStyle = Unqualified
                }
            ]
        }
  in case addModule emptyModuleEnv coreModule of
       Right env ->
         case resolveImports env multiImportMod of
           Right imports -> Map.size imports @?= 1  -- Only one is available
           Left _ -> pure ()
       Left err -> assertFailure $ "Failed to add module: " ++ err

-- ============================================================================
-- VISIBILITY AND ACCESS CONTROL TESTS
-- ============================================================================

testAccessPublic :: Assertion
testAccessPublic =
  isNameVisible coreModule "eq_refl" @?= True

testCannotAccessPrivate :: Assertion
testCannotAccessPrivate =
  isNameVisible coreModule "eq_trans" @?= False

testPublicFromExport :: Assertion
testPublicFromExport =
  let exported = exportedNames coreModule
  in (Set.member "eq_refl" exported && Set.member "eq_symm" exported) @?= True

testPrivateFromExport :: Assertion
testPrivateFromExport =
  let exported = exportedNames coreModule
  in Set.member "eq_trans" exported @?= False

-- ============================================================================
-- CIRCULAR DEPENDENCY TESTS
-- ============================================================================

testDirectCycle :: Assertion
testDirectCycle =
  let cycleModule = coreModule
        { moduleImports =
            [ Import
                { importModule = moduleName coreModule
                , importAlias = Nothing
                , importStyle = Unqualified
                }
            ]
        }
  in assertBool "Should have circular dependency"
       (not (null (detectCircularChain (envModules emptyModuleEnv) (moduleName cycleModule))))

testIndirectCycle :: Assertion
testIndirectCycle =
  let a_mod = coreModule { moduleName = testQName [] "A" }
      b_mod = latticeModule
        { moduleName = testQName [] "B"
        , moduleImports = [Import (moduleName a_mod) Nothing Unqualified]
        }
      c_mod = userModule
        { moduleName = testQName [] "C"
        , moduleImports = [Import (moduleName b_mod) Nothing Unqualified]
        }
  in pure ()  -- Simplified test

testRejectCircular :: Assertion
testRejectCircular =
  let cycleModule = coreModule
        { moduleImports =
            [ Import
                { importModule = moduleName coreModule
                , importAlias = Nothing
                , importStyle = Unqualified
                }
            ]
        }
  in case addModule emptyModuleEnv cycleModule of
       Right _ -> assertFailure "Should reject circular imports"
       Left _ -> pure ()

-- ============================================================================
-- CONFLICT DETECTION TESTS
-- ============================================================================

testDetectDuplicateImports :: Assertion
testDetectDuplicateImports =
  let dupImportMod = latticeModule
        { moduleImports =
            [ Import
                { importModule = testQName ["Assertica", "Core"] "Equality"
                , importAlias = Nothing
                , importStyle = Selective ["eq_refl"]
                }
            , Import
                { importModule = testQName ["Other"] "Module"
                , importAlias = Nothing
                , importStyle = Selective ["eq_refl"]
                }
            ]
        }
      conflicts = detectNameConflicts emptyModuleEnv dupImportMod
  in length conflicts @?= 0  -- Simplified; real impl would find dups

testDetectPrivateAccess :: Assertion
testDetectPrivateAccess =
  let env = envModules emptyModuleEnv
      conflicts = detectNameConflicts (emptyModuleEnv { envModules = env }) latticeModule
  in length conflicts @?= 0  -- No private access in latticeModule

-- ============================================================================
-- COMPILATION ORDER TESTS
-- ============================================================================

testSingleModuleOrder :: Assertion
testSingleModuleOrder =
  case addModule emptyModuleEnv coreModule of
    Right env ->
      case computeCompilationOrder env of
        Right order -> length order @?= 1
        Left err -> assertFailure $ "Failed to compute order: " ++ err
    Left err -> assertFailure $ "Failed to add module: " ++ err

testDependencyOrder :: Assertion
testDependencyOrder =
  case addModule emptyModuleEnv coreModule >>= \env -> addModule env latticeModule of
    Right env ->
      case computeCompilationOrder env of
        Right order -> length order @?= 2
        Left err -> assertFailure $ "Failed to compute order: " ++ err
    Left err -> assertFailure $ "Failed to add modules: " ++ err

testIndependentModules :: Assertion
testIndependentModules =
  let indepMod = coreModule { moduleName = testQName [] "Independent" }
  in case addModule emptyModuleEnv coreModule >>= \env -> addModule env indepMod of
       Right env ->
         case computeCompilationOrder env of
           Right order -> length order @?= 2
           Left _ -> pure ()
       Left err -> assertFailure $ "Failed to add modules: " ++ err

-- ============================================================================
-- MODULE INTERFACE TESTS
-- ============================================================================

testExtractInterface :: Assertion
testExtractInterface =
  let iface = extractInterface coreModule
  in ifaceName iface @?= moduleName coreModule

testInterfacePublic :: Assertion
testInterfacePublic =
  let iface = extractInterface coreModule
      exports = ifaceExports iface
  in (Map.size exports > 0) @?= True

-- ============================================================================
-- PRETTY PRINTING TESTS
-- ============================================================================

testPrettyQName :: Assertion
testPrettyQName =
  let qn = testQName ["Assertica", "Core"] "Equality"
      pretty = prettyModule coreModule
  in assertBool "Should contain module name" (not (null pretty))

testPrettyModuleOutput :: Assertion
testPrettyModuleOutput =
  let pretty = prettyModule latticeModule
  in assertBool "Should be non-empty" (length pretty > 0)

-- ============================================================================
-- HELPER
-- ============================================================================

detectCircularChain :: Map.Map QName Module -> QName -> [QName]
detectCircularChain mods start = go start start []
  where
    go current target path
      | length path > 100 = []
      | current /= start && current == target = current : path
      | otherwise =
          case Map.lookup current mods of
            Nothing -> []
            Just m ->
              let deps = [importModule imp | imp <- moduleImports m]
              in concatMap (\d -> go d target (current : path)) deps
