{-|
Module      : Test.Verify.CITests
Description : Tests for CI integration and batch reporting
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Tests covering:
- Batch verification from multiple files
- Aggregated reporting (JSON, CSV)
- Exit codes
- Code generation
- Deterministic batch reports
-}

module Test.Verify.CITests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import System.Exit (ExitCode(..))
import Assertica.Verify.CI
  ( BatchVerificationReport (..)
  , generateBatchReport
  , generateBatchJSON
  , generateBatchCSV
  , getExitCode
  , generateBatchHumanReadable
  )
import Assertica.Verify.Report
  ( VerificationReport (..)
  , StageResult (..)
  , Verdict (..)
  , SourceLocation (..)
  , ErrorDetail (..)
  , ErrorCategory (..)
  , mkStageResult
  , mkReport
  )

-- ============================================================================
-- CI INTEGRATION TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Verify.CI"
  [ testGroup "Batch Report Generation"
      [ testCase "empty batch report has zero files" $
          testEmptyBatchReport
      , testCase "batch report counts successful files" $
          testBatchReportCountsSuccessful
      , testCase "batch report counts failed files" $
          testBatchReportCountsFailed
      , testCase "batch report calculates pass rate" $
          testBatchReportPassRate
      , testCase "batch report calculates total duration" $
          testBatchReportTotalDuration
      ]
  , testGroup "Exit Codes"
      [ testCase "all success returns ExitSuccess" $
          testExitCodeSuccess
      , testCase "one failure returns ExitFailure 1" $
          testExitCodeOneFailure
      , testCase "multiple failures return correct count" $
          testExitCodeMultipleFailures
      ]
  , testGroup "JSON Batch Export"
      [ testCase "batch JSON contains summary" $
          testBatchJSONContainsSummary
      , testCase "batch JSON contains file count" $
          testBatchJSONContainsFileCount
      , testCase "batch JSON contains pass rate" $
          testBatchJSONContainsPassRate
      , testCase "batch JSON is well-formed" $
          testBatchJSONWellFormed
      , testCase "batch JSON is deterministic" $
          testBatchJSONDeterministic
      ]
  , testGroup "CSV Batch Export"
      [ testCase "batch CSV contains header" $
          testBatchCSVContainsHeader
      , testCase "batch CSV has one row per file" $
          testBatchCSVRowCount
      , testCase "batch CSV contains summary" $
          testBatchCSVContainsSummary
      ]
  , testGroup "Human-Readable Batch Report"
      [ testCase "batch text contains summary" $
          testBatchTextContainsSummary
      , testCase "batch text shows pass rate" $
          testBatchTextShowsPassRate
      ]
  , testGroup "Determinism"
      [ testCase "same reports produce same batch JSON" $
          testBatchJSONDeterminismExact
      , testCase "same reports produce same batch CSV" $
          testBatchCSVDeterminismExact
      ]
  ]

-- ============================================================================
-- BATCH REPORT GENERATION TESTS
-- ============================================================================

testEmptyBatchReport :: Assertion
testEmptyBatchReport = do
  batchReport <- generateBatchReport []
  bvrTotalFiles batchReport @?= 0
  bvrSuccessful batchReport @?= 0
  bvrFailed batchReport @?= 0

testBatchReportCountsSuccessful :: Assertion
testBatchReportCountsSuccessful = do
  let stage = mkStageResult "Test" 10.0
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report1, report2]
  bvrSuccessful batchReport @?= 2

testBatchReportCountsFailed :: Assertion
testBatchReportCountsFailed = do
  let stage = mkStageResult "Test" 10.0
      err = ErrorDetail SyntaxError "Error" Nothing Nothing Nothing
      failStage = StageFailed "Test" 10.0 err
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" failStage stage stage stage stage
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report1, report2]
  bvrFailed batchReport @?= 1

testBatchReportPassRate :: Assertion
testBatchReportPassRate = do
  let stage = mkStageResult "Test" 10.0
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report1, report2]
  bvrPassRate batchReport @?= 100.0

testBatchReportTotalDuration :: Assertion
testBatchReportTotalDuration = do
  let stage1 = mkStageResult "Test" 10.0
      stage2 = mkStageResult "Test" 10.0
      stage3 = mkStageResult "Test" 10.0
      stage4 = mkStageResult "Test" 10.0
      stage5 = mkStageResult "Test" 10.0
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" stage1 stage2 stage3 stage4 stage5
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" stage1 stage2 stage3 stage4 stage5
  batchReport <- generateBatchReport [report1, report2]
  bvrTotalDuration batchReport @?= 100.0

-- ============================================================================
-- EXIT CODE TESTS
-- ============================================================================

testExitCodeSuccess :: Assertion
testExitCodeSuccess = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  getExitCode batchReport @?= ExitSuccess

testExitCodeOneFailure :: Assertion
testExitCodeOneFailure = do
  let stage = mkStageResult "Test" 10.0
      err = ErrorDetail SyntaxError "Error" Nothing Nothing Nothing
      failStage = StageFailed "Test" 10.0 err
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" failStage stage stage stage stage
  batchReport <- generateBatchReport [report]
  getExitCode batchReport @?= ExitFailure 1

testExitCodeMultipleFailures :: Assertion
testExitCodeMultipleFailures = do
  let stage = mkStageResult "Test" 10.0
      err = ErrorDetail SyntaxError "Error" Nothing Nothing Nothing
      failStage = StageFailed "Test" 10.0 err
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" failStage stage stage stage stage
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" failStage stage stage stage stage
  batchReport <- generateBatchReport [report1, report2]
  getExitCode batchReport @?= ExitFailure 2

-- ============================================================================
-- JSON BATCH EXPORT TESTS
-- ============================================================================

testBatchJSONContainsSummary :: Assertion
testBatchJSONContainsSummary = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let json = generateBatchJSON batchReport
  ("summary" `elem` lines json) @?= True

testBatchJSONContainsFileCount :: Assertion
testBatchJSONContainsFileCount = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let json = generateBatchJSON batchReport
  ("totalFiles" `elem` concat (lines json)) @?= True

testBatchJSONContainsPassRate :: Assertion
testBatchJSONContainsPassRate = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let json = generateBatchJSON batchReport
  ("passRate" `elem` concat (lines json)) @?= True

testBatchJSONWellFormed :: Assertion
testBatchJSONWellFormed = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let json = generateBatchJSON batchReport
  ('{' `elem` json && '}' `elem` json) @?= True

testBatchJSONDeterministic :: Assertion
testBatchJSONDeterministic = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let json1 = generateBatchJSON batchReport
      json2 = generateBatchJSON batchReport
  json1 @?= json2

testBatchJSONDeterminismExact :: Assertion
testBatchJSONDeterminismExact = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  generateBatchJSON batchReport @?= generateBatchJSON batchReport

-- ============================================================================
-- CSV BATCH EXPORT TESTS
-- ============================================================================

testBatchCSVContainsHeader :: Assertion
testBatchCSVContainsHeader = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let csv = generateBatchCSV batchReport
  ("File,Module,Revision,Verdict,Duration" `elem` lines csv) @?= True

testBatchCSVRowCount :: Assertion
testBatchCSVRowCount = do
  let stage = mkStageResult "Test" 10.0
      report1 = mkReport "test1.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      report2 = mkReport "test2.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report1, report2]
  let csv = generateBatchCSV batchReport
      lineCount = length (lines csv)
  -- Header + 2 data rows + summary comment + summary rows
  (lineCount >= 3) @?= True

testBatchCSVContainsSummary :: Assertion
testBatchCSVContainsSummary = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let csv = generateBatchCSV batchReport
  ("Summary" `elem` csv) @?= True

-- ============================================================================
-- HUMAN-READABLE BATCH REPORT TESTS
-- ============================================================================

testBatchTextContainsSummary :: Assertion
testBatchTextContainsSummary = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let text = generateBatchHumanReadable batchReport
  ("BATCH VERIFICATION REPORT" `elem` lines text) @?= True

testBatchTextShowsPassRate :: Assertion
testBatchTextShowsPassRate = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  let text = generateBatchHumanReadable batchReport
  ("Pass Rate" `elem` concat (lines text)) @?= True

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testBatchCSVDeterminismExact :: Assertion
testBatchCSVDeterminismExact = do
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  batchReport <- generateBatchReport [report]
  generateBatchCSV batchReport @?= generateBatchCSV batchReport
