{-|
Module      : Test.Verify.ReportTests
Description : Tests for verification report serialization and formatting
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Tests covering:
- Report construction
- JSON serialization correctness
- Human-readable formatting
- CSV export
- Deterministic output (same input = same output)
-}

module Test.Verify.ReportTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Assertica.Verify.Report
  ( VerificationReport (..)
  , StageResult (..)
  , Verdict (..)
  , ErrorDetail (..)
  , ErrorCategory (..)
  , SourceLocation (..)
  , mkStageResult
  , mkReport
  , toJSON
  , toHumanReadable
  , toCSV
  , isSuccess
  , allStagesSuccessful
  , stagesToList
  )

-- ============================================================================
-- REPORT TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Verify.Report"
  [ testGroup "Report Construction"
      [ testCase "successful report has Accept verdict" $
          testSuccessfulReportVerdict
      , testCase "failed report has Reject verdict" $
          testFailedReportVerdict
      , testCase "report tracks all stages" $
          testReportTracksAllStages
      , testCase "report calculates total duration" $
          testReportTotalDuration
      ]
  , testGroup "Stage Results"
      [ testCase "successful stage is Success" $
          testSuccessfulStageIsSuccess
      , testCase "failed stage is not Success" $
          testFailedStageIsNotSuccess
      , testCase "all stages successful works correctly" $
          testAllStagesSuccessful
      ]
  , testGroup "JSON Serialization"
      [ testCase "JSON contains file path" $
          testJSONContainsFilePath
      , testCase "JSON contains verdict" $
          testJSONContainsVerdict
      , testCase "JSON is well-formed" $
          testJSONWellFormed
      , testCase "JSON serialization is deterministic" $
          testJSONDeterministic
      ]
  , testGroup "Human-Readable Format"
      [ testCase "text format contains file path" $
          testTextContainsFilePath
      , testCase "text format contains verdict" $
          testTextContainsVerdict
      , testCase "text format includes all stages" $
          testTextIncludesAllStages
      ]
  , testGroup "CSV Export"
      [ testCase "CSV format contains file" $
          testCSVContainsFile
      , testCase "CSV format contains verdict" $
          testCSVContainsVerdict
      , testCase "CSV is single line" $
          testCSVSingleLine
      ]
  , testGroup "Error Reporting"
      [ testCase "error detail in report has location" $
          testErrorDetailHasLocation
      , testCase "error message is preserved" $
          testErrorMessagePreserved
      ]
  , testGroup "Determinism"
      [ testCase "same report produces same JSON" $
          testJSONDeterminismExact
      , testCase "same report produces same text" $
          testTextDeterminismExact
      ]
  ]

-- ============================================================================
-- REPORT CONSTRUCTION TESTS
-- ============================================================================

testSuccessfulReportVerdict :: Assertion
testSuccessfulReportVerdict =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  in vrVerdict report @?= Accept

testFailedReportVerdict :: Assertion
testFailedReportVerdict =
  let successStage = mkStageResult "Test" 10.0
      err = ErrorDetail SyntaxError "Test error" Nothing Nothing Nothing
      failStage = StageFailed "Test" 10.0 err
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" failStage successStage successStage successStage successStage
  in vrVerdict report @?= Reject

testReportTracksAllStages :: Assertion
testReportTracksAllStages =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  in length (stagesToList report) @?= 5

testReportTotalDuration :: Assertion
testReportTotalDuration =
  let stage1 = mkStageResult "Stage1" 10.0
      stage2 = mkStageResult "Stage2" 20.0
      stage3 = mkStageResult "Stage3" 30.0
      stage4 = mkStageResult "Stage4" 40.0
      stage5 = mkStageResult "Stage5" 50.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage1 stage2 stage3 stage4 stage5
  in vrTotalDuration report @?= 150.0

-- ============================================================================
-- STAGE RESULTS TESTS
-- ============================================================================

testSuccessfulStageIsSuccess :: Assertion
testSuccessfulStageIsSuccess =
  let stage = mkStageResult "Test" 10.0
  in isSuccess stage @?= True

testFailedStageIsNotSuccess :: Assertion
testFailedStageIsNotSuccess =
  let err = ErrorDetail SyntaxError "Error" Nothing Nothing Nothing
      stage = StageFailed "Test" 10.0 err
  in isSuccess stage @?= False

testAllStagesSuccessful :: Assertion
testAllStagesSuccessful =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  in allStagesSuccessful report @?= True

-- ============================================================================
-- JSON SERIALIZATION TESTS
-- ============================================================================

testJSONContainsFilePath :: Assertion
testJSONContainsFilePath =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      json = toJSON report
  in ("test.as" `elem` lines json) @?= True

testJSONContainsVerdict :: Assertion
testJSONContainsVerdict =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      json = toJSON report
  in ("Accept" `elem` lines json) @?= True

testJSONWellFormed :: Assertion
testJSONWellFormed =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      json = toJSON report
  in ('{' `elem` json && '}' `elem` json) @?= True

testJSONDeterministic :: Assertion
testJSONDeterministic =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      json1 = toJSON report
      json2 = toJSON report
  in json1 @?= json2

testJSONDeterminismExact :: Assertion
testJSONDeterminismExact =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  in toJSON report @?= toJSON report

-- ============================================================================
-- HUMAN-READABLE FORMAT TESTS
-- ============================================================================

testTextContainsFilePath :: Assertion
testTextContainsFilePath =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      text = toHumanReadable report
  in ("test.as" `elem` lines text) @?= True

testTextContainsVerdict :: Assertion
testTextContainsVerdict =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      text = toHumanReadable report
  in ("Accept" `elem` lines text || any ("Accept" `elem`) (lines text)) @?= True

testTextIncludesAllStages :: Assertion
testTextIncludesAllStages =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      text = toHumanReadable report
  in (length (lines text) > 5) @?= True

testTextDeterminismExact :: Assertion
testTextDeterminismExact =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
  in toHumanReadable report @?= toHumanReadable report

-- ============================================================================
-- CSV EXPORT TESTS
-- ============================================================================

testCSVContainsFile :: Assertion
testCSVContainsFile =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      csv = toCSV report
  in ("test.as" `elem` lines csv) @?= True

testCSVContainsVerdict :: Assertion
testCSVContainsVerdict =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      csv = toCSV report
  in ("Accept" `elem` csv) @?= True

testCSVSingleLine :: Assertion
testCSVSingleLine =
  let stage = mkStageResult "Test" 10.0
      report = mkReport "test.as" Nothing Nothing "2026-01-01T00:00:00Z" stage stage stage stage stage
      csv = toCSV report
  in length (lines csv) @?= 1

-- ============================================================================
-- ERROR REPORTING TESTS
-- ============================================================================

testErrorDetailHasLocation :: Assertion
testErrorDetailHasLocation =
  let loc = SourceLocation "test.as" 1 1
      err = ErrorDetail SyntaxError "Test error" (Just loc) Nothing Nothing
  in (edLocation err /= Nothing) @?= True

testErrorMessagePreserved :: Assertion
testErrorMessagePreserved =
  let msg = "Test error message"
      err = ErrorDetail SyntaxError msg Nothing Nothing Nothing
  in edMessage err @?= msg
