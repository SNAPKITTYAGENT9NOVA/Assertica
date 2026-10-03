{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Verify.Report
Description : Verification report structures and serialization
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

The VerificationReport captures the complete result of verifying a file or module,
including the results at each pipeline stage, detailed errors, and the final verdict.

DESIGN PRINCIPLES:
1. Comprehensive: Captures all verification state at each stage
2. Deterministic: Same input produces identical report
3. Machine-readable: JSON serialization for CI consumption
4. Human-readable: Clear text format with explanations
5. Actionable: Every error includes source location and context
-}

module Assertica.Verify.Report
  ( -- * Main report types
    VerificationReport (..)
  , StageResult (..)
  , Verdict (..)
  , SourceLocation (..)

    -- * Error types
  , ErrorDetail (..)
  , ErrorCategory (..)

    -- * Report builders and serialization
  , mkReport
  , mkStageResult
  , toJSON
  , toHumanReadable
  , toCSV

    -- * Utility functions
  , isSuccess
  , allStagesSuccessful
  , stagesToList
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Ord (comparing)

-- ============================================================================
-- SOURCE LOCATION
-- ============================================================================

{-| Source location with file and line information.
-}
data SourceLocation = SourceLocation
  { slFile   :: String
  , slLine   :: Int
  , slColumn :: Int
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- ERROR TYPES
-- ============================================================================

{-| Error category for classification.
-}
data ErrorCategory
  = SyntaxError       -- ^ Parse failed (lexer/parser)
  | TypeError         -- ^ Type checker rejected
  | ProofObligation   -- ^ Proof missing or invalid
  | ModuleError       -- ^ Circular dependency, missing import
  | TerminationError  -- ^ Recursion not structurally decreasing
  | PositivityError   -- ^ Inductive type not strictly positive
  | CodeGenError      -- ^ Code generation failed
  | UnknownError      -- ^ Unexpected error
  deriving (Eq, Show, Ord, Generic)

{-| Detailed error information.
-}
data ErrorDetail = ErrorDetail
  { edCategory    :: ErrorCategory
  , edMessage     :: String
  , edLocation    :: Maybe SourceLocation
  , edContext     :: Maybe String    -- ^ Source code context
  , edSuggestedFix :: Maybe String   -- ^ Fix suggestion
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- STAGE RESULTS
-- ============================================================================

{-| Result of a single verification stage.
-}
data StageResult
  = StageOK
      { srStageName :: String
      , srDuration :: Double   -- ^ Milliseconds
      }
  | StageFailed
      { srStageName :: String
      , srDuration :: Double
      , srError :: ErrorDetail
      }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- FINAL VERDICT
-- ============================================================================

{-| Final verification verdict.
-}
data Verdict
  = Accept   -- ^ Module verified successfully
  | Reject   -- ^ Module failed verification
  deriving (Eq, Show, Ord, Generic)

-- ============================================================================
-- VERIFICATION REPORT
-- ============================================================================

{-| Complete verification report for a file/module.
-}
data VerificationReport = VerificationReport
  { vrFile              :: String              -- ^ Input file path
  , vrModule            :: Maybe String        -- ^ Module name (if applicable)
  , vrRevision          :: Maybe String        -- ^ Git revision or hash
  , vrTimestamp         :: String              -- ^ ISO 8601 timestamp
  , vrParseResult       :: StageResult         -- ^ Lexer/Parser stage
  , vrElaborationResult :: StageResult         -- ^ Elaboration stage
  , vrTypeCheckResult   :: StageResult         -- ^ Type checking stage
  , vrProofCheckResult  :: StageResult         -- ^ Proof verification stage
  , vrCodeGenResult     :: StageResult         -- ^ Code generation stage
  , vrVerdict           :: Verdict             -- ^ Final verdict
  , vrTotalDuration     :: Double              -- ^ Total time in milliseconds
  , vrAdditionalErrors  :: [ErrorDetail]       -- ^ Non-fatal errors or warnings
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- REPORT CONSTRUCTION
-- ============================================================================

{-| Create a successful stage result.
-}
mkStageResult :: String -> Double -> StageResult
mkStageResult name duration = StageOK name duration

{-| Create a failed stage result.
-}
mkFailedStageResult :: String -> Double -> ErrorDetail -> StageResult
mkFailedStageResult name duration err = StageFailed name duration err

{-| Create a verification report.
-}
mkReport
  :: String              -- ^ File path
  -> Maybe String        -- ^ Module name
  -> Maybe String        -- ^ Revision
  -> String              -- ^ Timestamp
  -> StageResult         -- ^ Parse result
  -> StageResult         -- ^ Elaboration result
  -> StageResult         -- ^ Type check result
  -> StageResult         -- ^ Proof check result
  -> StageResult         -- ^ Code gen result
  -> VerificationReport
mkReport file modName rev ts pr er tr pcr cg =
  let stages = [pr, er, tr, pcr, cg]
      verdict = if all isSuccess stages then Accept else Reject
      totalDuration = sum [stageDuration s | s <- stages]
  in VerificationReport
       { vrFile = file
       , vrModule = modName
       , vrRevision = rev
       , vrTimestamp = ts
       , vrParseResult = pr
       , vrElaborationResult = er
       , vrTypeCheckResult = tr
       , vrProofCheckResult = pcr
       , vrCodeGenResult = cg
       , vrVerdict = verdict
       , vrTotalDuration = totalDuration
       , vrAdditionalErrors = []
       }

-- ============================================================================
-- QUERY FUNCTIONS
-- ============================================================================

{-| Check if a stage result is successful.
-}
isSuccess :: StageResult -> Bool
isSuccess StageOK{} = True
isSuccess StageFailed{} = False

{-| Check if all stages in a report were successful.
-}
allStagesSuccessful :: VerificationReport -> Bool
allStagesSuccessful vr =
  all isSuccess
    [ vrParseResult vr
    , vrElaborationResult vr
    , vrTypeCheckResult vr
    , vrProofCheckResult vr
    , vrCodeGenResult vr
    ]

{-| Get the duration of a stage result.
-}
stageDuration :: StageResult -> Double
stageDuration StageOK{srDuration=d} = d
stageDuration StageFailed{srDuration=d} = d

{-| Get stage name.
-}
stageName :: StageResult -> String
stageName StageOK{srStageName=n} = n
stageName StageFailed{srStageName=n} = n

{-| Convert report to list of stages.
-}
stagesToList :: VerificationReport -> [StageResult]
stagesToList vr =
  [ vrParseResult vr
  , vrElaborationResult vr
  , vrTypeCheckResult vr
  , vrProofCheckResult vr
  , vrCodeGenResult vr
  ]

-- ============================================================================
-- SERIALIZATION: JSON
-- ============================================================================

{-| Serialize report to JSON format.
Deterministic ordering ensures identical input produces identical JSON.
-}
toJSON :: VerificationReport -> String
toJSON vr =
  "{\n"
  ++ "  \"file\": \"" ++ escapeJSON (vrFile vr) ++ "\",\n"
  ++ "  \"module\": " ++ maybe "null" (\m -> "\"" ++ escapeJSON m ++ "\"") (vrModule vr) ++ ",\n"
  ++ "  \"revision\": " ++ maybe "null" (\r -> "\"" ++ escapeJSON r ++ "\"") (vrRevision vr) ++ ",\n"
  ++ "  \"timestamp\": \"" ++ escapeJSON (vrTimestamp vr) ++ "\",\n"
  ++ "  \"verdict\": \"" ++ show (vrVerdict vr) ++ "\",\n"
  ++ "  \"totalDuration\": " ++ show (vrTotalDuration vr) ++ ",\n"
  ++ "  \"stages\": [\n"
  ++ intercalate ",\n" (map stageToJSON (stagesToList vr))
  ++ "\n  ],\n"
  ++ "  \"additionalErrors\": [\n"
  ++ intercalate ",\n" (map errorToJSON (vrAdditionalErrors vr))
  ++ "\n  ]\n"
  ++ "}"

{-| Serialize a stage result to JSON.
-}
stageToJSON :: StageResult -> String
stageToJSON (StageOK name duration) =
  "    {\n"
  ++ "      \"name\": \"" ++ escapeJSON name ++ "\",\n"
  ++ "      \"status\": \"OK\",\n"
  ++ "      \"duration\": " ++ show duration ++ "\n"
  ++ "    }"
stageToJSON (StageFailed name duration err) =
  "    {\n"
  ++ "      \"name\": \"" ++ escapeJSON name ++ "\",\n"
  ++ "      \"status\": \"FAILED\",\n"
  ++ "      \"duration\": " ++ show duration ++ ",\n"
  ++ "      \"error\": " ++ errorToJSON err ++ "\n"
  ++ "    }"

{-| Serialize an error to JSON.
-}
errorToJSON :: ErrorDetail -> String
errorToJSON err =
  "{\n"
  ++ "        \"category\": \"" ++ show (edCategory err) ++ "\",\n"
  ++ "        \"message\": \"" ++ escapeJSON (edMessage err) ++ "\",\n"
  ++ "        \"location\": " ++ locationToJSON (edLocation err) ++ ",\n"
  ++ "        \"context\": " ++ maybe "null" (\c -> "\"" ++ escapeJSON c ++ "\"") (edContext err) ++ ",\n"
  ++ "        \"suggestedFix\": " ++ maybe "null" (\f -> "\"" ++ escapeJSON f ++ "\"") (edSuggestedFix err) ++ "\n"
  ++ "      }"

{-| Serialize source location to JSON.
-}
locationToJSON :: Maybe SourceLocation -> String
locationToJSON Nothing = "null"
locationToJSON (Just sl) =
  "{\n"
  ++ "          \"file\": \"" ++ escapeJSON (slFile sl) ++ "\",\n"
  ++ "          \"line\": " ++ show (slLine sl) ++ ",\n"
  ++ "          \"column\": " ++ show (slColumn sl) ++ "\n"
  ++ "        }"

{-| Escape special JSON characters.
-}
escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

-- ============================================================================
-- SERIALIZATION: HUMAN-READABLE TEXT
-- ============================================================================

{-| Serialize report to human-readable text format.
-}
toHumanReadable :: VerificationReport -> String
toHumanReadable vr =
  unlines
    [ "=========================================="
    , "VERIFICATION REPORT"
    , "=========================================="
    , ""
    , "File:      " ++ vrFile vr
    , "Module:    " ++ maybe "(none)" id (vrModule vr)
    , "Revision:  " ++ maybe "(unknown)" id (vrRevision vr)
    , "Timestamp: " ++ vrTimestamp vr
    , ""
    , "=========================================="
    , "VERIFICATION STAGES"
    , "=========================================="
    , ""
    , formatStages (stagesToList vr)
    , ""
    , "=========================================="
    , "FINAL VERDICT: " ++ show (vrVerdict vr)
    , "TOTAL TIME: " ++ show (vrTotalDuration vr) ++ " ms"
    , "=========================================="
    ] ++ (if null (vrAdditionalErrors vr) then "" else "\nADDITIONAL MESSAGES:\n" ++ formatErrors (vrAdditionalErrors vr))

{-| Format stages for human-readable output.
-}
formatStages :: [StageResult] -> String
formatStages = unlines . zipWith formatStage [1..]
  where
    formatStage i (StageOK name duration) =
      "  [" ++ show i ++ "] " ++ padTo 20 name ++ " : OK (" ++ show duration ++ " ms)"
    formatStage i (StageFailed name duration err) =
      "  [" ++ show i ++ "] " ++ padTo 20 name ++ " : FAILED (" ++ show duration ++ " ms)\n" ++
      "      Error: " ++ edMessage err ++ "\n" ++
      locString (edLocation err)

{-| Format errors for human-readable output.
-}
formatErrors :: [ErrorDetail] -> String
formatErrors = unlines . map formatError
  where
    formatError err =
      "  - [" ++ show (edCategory err) ++ "] " ++ edMessage err
      ++ locString (edLocation err)
      ++ case edSuggestedFix err of
           Just fix -> "\n    Suggestion: " ++ fix
           Nothing -> ""

{-| Format location string.
-}
locString :: Maybe SourceLocation -> String
locString Nothing = ""
locString (Just sl) =
  " (at " ++ slFile sl ++ ":" ++ show (slLine sl) ++ ":" ++ show (slColumn sl) ++ ")"

{-| Pad string to specified width.
-}
padTo :: Int -> String -> String
padTo n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================================
-- SERIALIZATION: CSV (for batch reporting)
-- ============================================================================

{-| Serialize report to CSV format (single row per file).
-}
toCSV :: VerificationReport -> String
toCSV vr =
  escapeCSV (vrFile vr) ++ ","
  ++ escapeCSV (maybe "" id (vrModule vr)) ++ ","
  ++ escapeCSV (maybe "" id (vrRevision vr)) ++ ","
  ++ show (vrVerdict vr) ++ ","
  ++ show (vrTotalDuration vr)

{-| CSV header for batch reports.
-}
csvHeader :: String
csvHeader = "File,Module,Revision,Verdict,Duration"

{-| Escape special CSV characters.
-}
escapeCSV :: String -> String
escapeCSV s
  | any (`elem` s) ",\"\n" = "\"" ++ concatMap escapeChar s ++ "\""
  | otherwise = s
  where
    escapeChar '"' = "\"\""
    escapeChar c = [c]
