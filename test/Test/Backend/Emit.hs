{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Emit
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

import Assertica.Backend.CodeGen
import Assertica.Backend.Emit

-- ============================================================================
-- TEST SUITE
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Backend.Emit"
  [ testGroup "Module Header"
      [ testCase "Simple module name" testSimpleModuleHeader
      , testCase "Qualified module name" testQualifiedModuleHeader
      ]
  , testGroup "Imports"
      [ testCase "Empty imports" testEmptyImports
      , testCase "Single import" testSingleImport
      , testCase "Multiple imports" testMultipleImports
      ]
  , testGroup "Type Signatures"
      [ testCase "Format type signature" testTypeSignature
      ]
  , testGroup "Definitions"
      [ testCase "Format simple definition" testSimpleDefinition
      , testCase "Definition with type signature" testDefinitionWithType
      ]
  , testGroup "Module Emission"
      [ testCase "Emit empty module" testEmptyModule
      , testCase "Emit module with definition" testModuleWithDefinition
      ]
  , testGroup "Configuration"
      [ testCase "Default config values" testDefaultConfig
      ]
  ]

-- ============================================================================
-- UNIT TESTS: MODULE HEADER
-- ============================================================================

testSimpleModuleHeader :: Assertion
testSimpleModuleHeader = do
  let header = formatModuleHeader ["Main"]
  assertBool "Should contain module" ("module" `T.isInfixOf` header)
  assertBool "Should contain Main" ("Main" `T.isInfixOf` header)
  assertBool "Should contain where" ("where" `T.isInfixOf` header)

testQualifiedModuleHeader :: Assertion
testQualifiedModuleHeader = do
  let header = formatModuleHeader ["Assertica", "Core", "AST"]
  assertBool "Should contain Assertica" ("Assertica" `T.isInfixOf` header)
  assertBool "Should contain Core" ("Core" `T.isInfixOf` header)
  assertBool "Should contain AST" ("AST" `T.isInfixOf` header)
  assertBool "Should use dots" ("." `T.isInfixOf` header)

-- ============================================================================
-- UNIT TESTS: IMPORTS
-- ============================================================================

testEmptyImports :: Assertion
testEmptyImports = do
  let imports = formatImports defaultEmitConfig []
  assertEqual "Should be empty list" [] imports

testSingleImport :: Assertion
testSingleImport = do
  let imports = formatImports defaultEmitConfig ["Data.List"]
  assertBool "Should contain import" ("import" `T.isInfixOf` (head imports))
  assertBool "Should contain module" ("Data.List" `T.isInfixOf` (head imports))

testMultipleImports :: Assertion
testMultipleImports = do
  let imports = formatImports defaultEmitConfig
                  ["Data.List", "Data.Map", "Control.Monad"]
  assertEqual "Should have 3 imports" 3 (length imports)

-- ============================================================================
-- UNIT TESTS: TYPE SIGNATURES
-- ============================================================================

testTypeSignature :: Assertion
testTypeSignature = do
  let sig = formatTypeSignature "myFunction" "Int -> Bool"
  assertBool "Should contain name" ("myFunction" `T.isInfixOf` sig)
  assertBool "Should contain type" ("Int" `T.isInfixOf` sig)
  assertBool "Should contain ::" ("::" `T.isInfixOf` sig)

-- ============================================================================
-- UNIT TESTS: DEFINITIONS
-- ============================================================================

testSimpleDefinition :: Assertion
testSimpleDefinition = do
  let def = formatDefinition "x" "42"
  assertBool "Should contain name" ("x" `T.isInfixOf` def)
  assertBool "Should contain equals" ("=" `T.isInfixOf` def)
  assertBool "Should contain body" ("42" `T.isInfixOf` def)

testDefinitionWithType :: Assertion
testDefinitionWithType = do
  let config = defaultEmitConfig
  let def = HaskellDefinition
        { hdName = "add"
        , hdTypeSignature = Just "Int -> Int -> Int"
        , hdBody = "\\x y -> x + y"
        , hdSourceLocation = Nothing
        }
  let emitted = emitHaskellDefinition config def
  assertBool "Should contain type sig" ("Int" `T.isInfixOf` emitted)
  assertBool "Should contain definition" ("add" `T.isInfixOf` emitted)
  assertBool "Should contain body" ("+" `T.isInfixOf` emitted)

-- ============================================================================
-- UNIT TESTS: MODULE EMISSION
-- ============================================================================

testEmptyModule :: Assertion
testEmptyModule = do
  let mod = HaskellModule
        { hmName = ["Empty"]
        , hmImports = []
        , hmDefinitions = []
        , hmComments = []
        }
  let emitted = emitHaskellModule defaultEmitConfig mod
  assertBool "Should contain module" ("module" `T.isInfixOf` emitted)
  assertBool "Should contain Empty" ("Empty" `T.isInfixOf` emitted)

testModuleWithDefinition :: Assertion
testModuleWithDefinition = do
  let def = HaskellDefinition
        { hdName = "x"
        , hdTypeSignature = Just "Int"
        , hdBody = "42"
        , hdSourceLocation = Nothing
        }
  let mod = HaskellModule
        { hmName = ["Main"]
        , hmImports = ["Prelude"]
        , hmDefinitions = [def]
        , hmComments = []
        }
  let emitted = emitHaskellModule defaultEmitConfig mod
  assertBool "Should contain module" ("module" `T.isInfixOf` emitted)
  assertBool "Should contain imports" ("Prelude" `T.isInfixOf` emitted)
  assertBool "Should contain definition" ("x" `T.isInfixOf` emitted)
  assertBool "Should contain type sig" ("Int" `T.isInfixOf` emitted)
  assertBool "Should contain body" ("42" `T.isInfixOf` emitted)

-- ============================================================================
-- UNIT TESTS: CONFIGURATION
-- ============================================================================

testDefaultConfig :: Assertion
testDefaultConfig = do
  assertEqual "Indent size should be 2" 2 (ecIndentSize defaultEmitConfig)
  assertEqual "Line length should be 80" 80 (ecLineLength defaultEmitConfig)
  assertEqual "Add comments should be True" True (ecAddComments defaultEmitConfig)
  assertEqual "Prelude imports should be True" True (ecPreludeImports defaultEmitConfig)
