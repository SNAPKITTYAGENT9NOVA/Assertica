{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Verify.CI
Description : CI integration for batch verification and reporting
Copyright   : (c) 2026 Ahmad Ali Proofs
License     : MIT

Provides functionality for CI systems to verify multiple files and generate
aggregated reports suitable for consumption by CI platforms.

Features:
- Batch verification of all .as files in a directory
- Aggregated JSON and CSV reporting
- Deterministic exit codes (0 = all pass, non-zero = any fail)
- Optional Haskell code generation for verified modules
- Timeout handling and robustness

DESIGN PRINCIPLES:
1. CI-friendly: Structured output, predictable exit codes
2. Deterministic: Same source tree produces identical report
3. Robust: Handles missing files, timeouts, I/O errors gracefully
4. Efficient: Process files in parallel where possible (future)
-}

module Assertica.Verify.CI
  ( -- * Main CI entry point
    ciVerify

    -- * Batch reporting
  , BatchVerificationReport (..)
  , generateBatchReport
  , generateBatchJSON
  , generateBatchCSV

    -- * Exit codes
  , getExitCode
  , ExitCode (..)

    -- * Code generation
  , generateHaskellForDirectory
  , generateHaskellForReport
  ) where

import Assertica.Verify.Pipeline (verifyDirectory)
import Assertica.Verify.Report
  ( VerificationReport (..)
  , toJSON
  , toHumanReadable
  , toCSV
  , csvHeader
  , Verdict (..)
  )
import Control.Monad (filterM)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.FilePath (takeBaseName)
import System.Directory (doesDirectoryExist, listDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))

-- ============================================================================
-- BATCH VERIFICATION REPORT
-- ============================================================================

{-| Report aggregating results from multiple files.
-}
data BatchVerificationReport = BatchVerificationReport
  { bvrReports         :: [VerificationReport]  -- ^ Individual file reports
  , bvrTotalFiles      :: Int                    -- ^ Number of files verified
  , bvrSuccessful      :: Int                    -- ^ Number of successful verifications
  , bvrFailed          :: Int                    -- ^ Number of failed verifications
  , bvrTotalDuration   :: Double                 -- ^ Total time in milliseconds
  , bvrTimestamp       :: String                 -- ^ Report timestamp
  , bvrPassRate        :: Double                 -- ^ Success percentage
  }
  deriving (Show)

-- ============================================================================
-- CI VERIFICATION
-- ============================================================================

{-| Main entry point for CI verification.
Verifies all .as files in a directory and returns batch report.
-}
ciVerify :: FilePath -> IO BatchVerificationReport
ciVerify dirPath = do
  reports <- verifyDirectory dirPath
  generateBatchReport reports

{-| Generate batch report from individual reports.
-}
generateBatchReport :: [VerificationReport] -> IO BatchVerificationReport
generateBatchReport reports = do
  let total = length reports
      successful = length [r | r <- reports, vrVerdict r == Accept]
      failed = total - successful
      totalDuration = sum [vrTotalDuration r | r <- reports]
      passRate = if total == 0 then 0.0 else fromIntegral successful / fromIntegral total * 100.0
      timestamp = if null reports then "unknown" else vrTimestamp (head reports)

  return $ BatchVerificationReport
    { bvrReports = reports
    , bvrTotalFiles = total
    , bvrSuccessful = successful
    , bvrFailed = failed
    , bvrTotalDuration = totalDuration
    , bvrTimestamp = timestamp
    , bvrPassRate = passRate
    }

-- ============================================================================
-- EXIT CODES
-- ============================================================================

{-| Get exit code based on verification result.
Deterministic: Always same result for same input.
-}
getExitCode :: BatchVerificationReport -> ExitCode
getExitCode report
  | bvrFailed report == 0 = ExitSuccess
  | otherwise = ExitFailure (bvrFailed report)

-- ============================================================================
-- BATCH JSON REPORTING
-- ============================================================================

{-| Generate JSON report for batch verification.
Format:
{
  "timestamp": "...",
  "totalFiles": N,
  "successful": N,
  "failed": N,
  "passRate": X.X,
  "totalDuration": N.N,
  "files": [
    { ... full report JSON for each file ... }
  ]
}
-}
generateBatchJSON :: BatchVerificationReport -> String
generateBatchJSON report =
  "{\n"
  ++ "  \"timestamp\": \"" ++ bvrTimestamp report ++ "\",\n"
  ++ "  \"summary\": {\n"
  ++ "    \"totalFiles\": " ++ show (bvrTotalFiles report) ++ ",\n"
  ++ "    \"successful\": " ++ show (bvrSuccessful report) ++ ",\n"
  ++ "    \"failed\": " ++ show (bvrFailed report) ++ ",\n"
  ++ "    \"passRate\": " ++ show (bvrPassRate report) ++ ",\n"
  ++ "    \"totalDuration\": " ++ show (bvrTotalDuration report) ++ "\n"
  ++ "  },\n"
  ++ "  \"files\": [\n"
  ++ intercalateJSON (map toJSON (sortReports (bvrReports report)))
  ++ "\n  ]\n"
  ++ "}"

{-| Join JSON objects with commas.
-}
intercalateJSON :: [String] -> String
intercalateJSON [] = ""
intercalateJSON [x] = x
intercalateJSON (x:xs) = x ++ ",\n" ++ intercalateJSON xs

{-| Sort reports deterministically (by file path).
-}
sortReports :: [VerificationReport] -> [VerificationReport]
sortReports = sortBy (comparing vrFile)

-- ============================================================================
-- BATCH CSV REPORTING
-- ============================================================================

{-| Generate CSV report for batch verification.
Format: CSV with headers and one row per file.
-}
generateBatchCSV :: BatchVerificationReport -> String
generateBatchCSV report =
  unlines $
    [csvHeader] ++
    [toCSV r | r <- sortReports (bvrReports report)] ++
    ["# Summary"]  ++
    ["# Total: " ++ show (bvrTotalFiles report)]  ++
    ["# Successful: " ++ show (bvrSuccessful report)]  ++
    ["# Failed: " ++ show (bvrFailed report)]  ++
    ["# PassRate: " ++ show (bvrPassRate report) ++ "%"]

-- ============================================================================
-- HUMAN-READABLE BATCH REPORT
-- ============================================================================

{-| Generate human-readable batch report.
-}
generateBatchHumanReadable :: BatchVerificationReport -> String
generateBatchHumanReadable report =
  unlines
    [ "=========================================="
    , "BATCH VERIFICATION REPORT"
    , "=========================================="
    , ""
    , "Timestamp: " ++ bvrTimestamp report
    , "Total Files: " ++ show (bvrTotalFiles report)
    , "Successful: " ++ show (bvrSuccessful report)
    , "Failed: " ++ show (bvrFailed report)
    , "Pass Rate: " ++ show (bvrPassRate report) ++ "%"
    , "Total Duration: " ++ show (bvrTotalDuration report) ++ " ms"
    , ""
    , "=========================================="
    , "FILE RESULTS"
    , "=========================================="
    , ""
    ] ++
    [formatFileResult r | r <- sortReports (bvrReports report)]

{-| Format single file result.
-}
formatFileResult :: VerificationReport -> String
formatFileResult vr =
  let status = case vrVerdict vr of
                 Accept -> "PASS"
                 Reject -> "FAIL"
      marker = if status == "PASS" then "✓" else "✗"
  in marker ++ " " ++ vrFile vr ++ " [" ++ status ++ ", " ++ show (vrTotalDuration vr) ++ "ms]"

-- ============================================================================
-- CODE GENERATION FOR VERIFIED MODULES
-- ============================================================================

{-| Generate Haskell code for all successfully verified modules in a directory.
-}
generateHaskellForDirectory :: FilePath -> FilePath -> IO ()
generateHaskellForDirectory sourceDir outputDir = do
  reports <- verifyDirectory sourceDir
  generateHaskellForReport reports outputDir

{-| Generate Haskell code for verified modules from reports.
For each successfully verified file, generates corresponding Haskell code.
-}
generateHaskellForReport :: [VerificationReport] -> FilePath -> IO ()
generateHaskellForReport reports outputDir = do
  createDirectoryIfMissing True outputDir

  -- Filter to successful verifications
  let successful = [r | r <- reports, vrVerdict r == Accept]

  -- Generate Haskell for each
  mapM_ (generateHaskellForFile outputDir) successful

{-| Generate Haskell code for a single verified module.
-}
generateHaskellForFile :: FilePath -> VerificationReport -> IO ()
generateHaskellForFile outputDir vr = do
  -- Placeholder: real implementation would extract code from verification
  -- For now, we just create a stub
  let outputFile = outputDir ++ "/" ++ takeBaseName (vrFile vr) ++ ".hs"
  writeFile outputFile $ generateHaskellStub vr

{-| Generate Haskell stub for a module.
Placeholder implementation.
-}
generateHaskellStub :: VerificationReport -> String
generateHaskellStub vr =
  "-- Generated from " ++ vrFile vr ++ "\n" ++
  "-- Verified at " ++ vrTimestamp vr ++ "\n" ++
  "-- Status: " ++ show (vrVerdict vr) ++ "\n" ++
  "\n" ++
  "module Generated where\n" ++
  "\n" ++
  "-- Generated code placeholder\n"
