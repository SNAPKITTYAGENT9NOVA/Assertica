{-|
Module      : Test.Verify.ErrorReportTests
Description : Tests for error categorization and formatting
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

Tests covering:
- Error categorization from messages
- Error formatting and display
- Source location handling
- Fix suggestion generation
- Context extraction
-}

module Test.Verify.ErrorReportTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Assertica.Verify.ErrorReport
  ( categorizeError
  , formatError
  , formatErrorLocation
  , suggestFix
  , suggestParseError
  , suggestTypeError
  , suggestProofError
  , suggestModuleError
  , suggestTerminationError
  , suggestPositivityError
  , buildContext
  )
import Assertica.Verify.Report
  ( ErrorDetail (..)
  , ErrorCategory (..)
  , SourceLocation (..)
  )

-- ============================================================================
-- ERROR CATEGORIZATION TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Verify.ErrorReport"
  [ testGroup "Error Categorization"
      [ testCase "parse error message categorized as SyntaxError" $
          testCategorizeParseError
      , testCase "type error message categorized as TypeError" $
          testCategorizeTypeError
      , testCase "proof error categorized as ProofObligation" $
          testCategorizeProofError
      , testCase "circular dependency categorized as ModuleError" $
          testCategorizeModuleError
      , testCase "termination error categorized correctly" $
          testCategorizeTerminationError
      , testCase "positivity error categorized correctly" $
          testCategorizePositivityError
      , testCase "codegen error categorized correctly" $
          testCategorizeCodeGenError
      , testCase "unknown error defaults to UnknownError" $
          testCategorizeUnknownError
      ]
  , testGroup "Error Formatting"
      [ testCase "format error includes category" $
          testFormatErrorIncludesCategory
      , testCase "format error includes message" $
          testFormatErrorIncludesMessage
      , testCase "format error includes location when present" $
          testFormatErrorIncludesLocation
      , testCase "format error deterministic" $
          testFormatErrorDeterministic
      ]
  , testGroup "Location Formatting"
      [ testCase "location format includes file" $
          testLocationFormatIncludesFile
      , testCase "location format includes line" $
          testLocationFormatIncludesLine
      , testCase "location format includes column" $
          testLocationFormatIncludesColumn
      ]
  , testGroup "Fix Suggestions"
      [ testCase "parse error suggests fix" $
          testParseSuggestsFix
      , testCase "type error suggests fix" $
          testTypeSuggestsFix
      , testCase "proof error suggests fix" $
          testProofSuggestsFix
      , testCase "module error suggests fix" $
          testModuleSuggestsFix
      , testCase "termination error suggests fix" $
          testTerminationSuggestsFix
      , testCase "positivity error suggests fix" $
          testPositivitySuggestsFix
      ]
  , testGroup "Context Extraction"
      [ testCase "build context with valid location" $
          testBuildContextValid
      , testCase "build context with valid source" $
          testBuildContextValidSource
      ]
  , testGroup "Determinism"
      [ testCase "same error produces same category" $
          testCategorizeDeterministic
      , testCase "same error produces same format" $
          testFormatDeterministic
      ]
  ]

-- ============================================================================
-- ERROR CATEGORIZATION TESTS
-- ============================================================================

testCategorizeParseError :: Assertion
testCategorizeParseError =
  categorizeError "parse error at token" Nothing @?= SyntaxError

testCategorizeTypeError :: Assertion
testCategorizeTypeError =
  categorizeError "type mismatch: expected Int but got Bool" Nothing @?= TypeError

testCategorizeProofError :: Assertion
testCategorizeProofError =
  categorizeError "proof obligation: assertion not proven" Nothing @?= ProofObligation

testCategorizeModuleError :: Assertion
testCategorizeModuleError =
  categorizeError "circular dependency detected" Nothing @?= ModuleError

testCategorizeTerminationError :: Assertion
testCategorizeTerminationError =
  categorizeError "termination: decreasing argument not found" Nothing @?= TerminationError

testCategorizePositivityError :: Assertion
testCategorizePositivityError =
  categorizeError "positivity: type parameter in negative position" Nothing @?= PositivityError

testCategorizeCodeGenError :: Assertion
testCategorizeCodeGenError =
  categorizeError "codegen failed: invalid Haskell output" Nothing @?= CodeGenError

testCategorizeUnknownError :: Assertion
testCategorizeUnknownError =
  categorizeError "something went wrong" Nothing @?= UnknownError

-- ============================================================================
-- ERROR FORMATTING TESTS
-- ============================================================================

testFormatErrorIncludesCategory :: Assertion
testFormatErrorIncludesCategory =
  let err = ErrorDetail SyntaxError "Test error" Nothing Nothing Nothing
      formatted = formatError err
  in ("SYNTAX" `elem` formatted) @?= True

testFormatErrorIncludesMessage :: Assertion
testFormatErrorIncludesMessage =
  let err = ErrorDetail SyntaxError "Test error" Nothing Nothing Nothing
      formatted = formatError err
  in ("Test error" `elem` formatted) @?= True

testFormatErrorIncludesLocation :: Assertion
testFormatErrorIncludesLocation =
  let loc = SourceLocation "test.as" 1 5
      err = ErrorDetail SyntaxError "Test error" (Just loc) Nothing Nothing
      formatted = formatError err
  in ("test.as" `elem` formatted) @?= True

testFormatErrorDeterministic :: Assertion
testFormatErrorDeterministic =
  let err = ErrorDetail SyntaxError "Test error" Nothing Nothing Nothing
      formatted1 = formatError err
      formatted2 = formatError err
  in formatted1 @?= formatted2

-- ============================================================================
-- LOCATION FORMATTING TESTS
-- ============================================================================

testLocationFormatIncludesFile :: Assertion
testLocationFormatIncludesFile =
  let loc = SourceLocation "test.as" 1 1
      formatted = formatErrorLocation loc
  in ("test.as" `elem` formatted) @?= True

testLocationFormatIncludesLine :: Assertion
testLocationFormatIncludesLine =
  let loc = SourceLocation "test.as" 42 1
      formatted = formatErrorLocation loc
  in ("42" `elem` formatted) @?= True

testLocationFormatIncludesColumn :: Assertion
testLocationFormatIncludesColumn =
  let loc = SourceLocation "test.as" 1 5
      formatted = formatErrorLocation loc
  in ("5" `elem` formatted) @?= True

-- ============================================================================
-- FIX SUGGESTION TESTS
-- ============================================================================

testParseSuggestsFix :: Assertion
testParseSuggestsFix =
  suggestParseError "unexpected token" @?= Just "Check for missing operators, mismatched brackets, or incorrect syntax."

testTypeSuggestsFix :: Assertion
testTypeSuggestsFix =
  case suggestTypeError "cannot unify Int with Bool" of
    Just msg -> ("Add explicit type annotation" `elem` msg) @?= True
    Nothing -> fail "Type error should suggest fix"

testProofSuggestsFix :: Assertion
testProofSuggestsFix =
  case suggestProofError "missing proof" of
    Just msg -> ("proof term" `elem` msg) @?= True
    Nothing -> fail "Proof error should suggest fix"

testModuleSuggestsFix :: Assertion
testModuleSuggestsFix =
  case suggestModuleError "circular dependency" of
    Just msg -> ("circular" `elem` msg) @?= True
    Nothing -> fail "Module error should suggest fix"

testTerminationSuggestsFix :: Assertion
testTerminationSuggestsFix =
  case suggestTerminationError "decreasing argument" of
    Just msg -> ("structural" `elem` msg) @?= True
    Nothing -> fail "Termination error should suggest fix"

testPositivitySuggestsFix :: Assertion
testPositivitySuggestsFix =
  case suggestPositivityError "negative occurrence" of
    Just msg -> ("negative" `elem` msg) @?= True
    Nothing -> fail "Positivity error should suggest fix"

-- ============================================================================
-- CONTEXT EXTRACTION TESTS
-- ============================================================================

testBuildContextValid :: Assertion
testBuildContextValid =
  let source = "assert x = x\nassert y = y\nassert z = z"
      loc = SourceLocation "test.as" 2 1
      ctx = buildContext source loc 1
  in length ctx > 0 @?= True

testBuildContextValidSource :: Assertion
testBuildContextValidSource =
  let source = "line 1\nline 2\nline 3"
      loc = SourceLocation "test.as" 2 1
      ctx = buildContext source loc 1
  in ("2" `elem` ctx) @?= True

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testCategorizeDeterministic :: Assertion
testCategorizeDeterministic =
  let msg = "parse error at position"
  in categorizeError msg Nothing @?= categorizeError msg Nothing

testFormatDeterministic :: Assertion
testFormatDeterministic =
  let err = ErrorDetail SyntaxError "Test" Nothing Nothing Nothing
  in formatError err @?= formatError err
