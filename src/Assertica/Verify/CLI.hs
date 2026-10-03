{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Verify.CLI
Description : Command-line interface for verification
Copyright   : (c) 2026 Ahmad Ali Parr
License     : MIT

Provides a command-line interface for running verification from the shell.

Usage:
  assertica-verify --file FILE [--output FORMAT] [--generate-haskell] [--strict]
  assertica-verify --dir DIR [--output FORMAT] [--generate-haskell] [--strict]

Options:
  --file FILE              Verify single file (required if --dir not given)
  --dir DIR                Verify all .as files in directory (required if --file not given)
  --output FORMAT          Output format: json, text, csv (default: text)
  --generate-haskell       Generate Haskell code for verified modules
  --strict                 Treat warnings as errors
  --help                   Show this help message

Exit codes:
  0                        All verifications passed
  N                        N verifications failed
-}

module Assertica.Verify.CLI
  ( -- * Main CLI entry point
    main

    -- * CLI argument parsing
  , CLIArgs (..)
  , OutputFormat (..)
  , parseCLIArgs
  , defaultCLIArgs

    -- * CLI execution
  , runCLI
  ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Assertica.Verify.Pipeline (verifyFile, verifyDirectory)
import Assertica.Verify.CI (ciVerify, generateBatchJSON, generateBatchCSV, getExitCode, generateBatchHumanReadable)
import Assertica.Verify.Report (toJSON, toHumanReadable, toCSV, Verdict(..))

-- ============================================================================
-- CLI TYPES
-- ============================================================================

{-| Output format specification.
-}
data OutputFormat
  = JSON
  | Text
  | CSV
  deriving (Eq, Show)

{-| Parsed command-line arguments.
-}
data CLIArgs = CLIArgs
  { argFile          :: Maybe FilePath       -- ^ File to verify
  , argDir           :: Maybe FilePath       -- ^ Directory to verify
  , argOutputFormat  :: OutputFormat         -- ^ Output format
  , argGenerateHS    :: Bool                 -- ^ Generate Haskell code
  , argStrict        :: Bool                 -- ^ Strict mode (warnings as errors)
  , argHelp          :: Bool                 -- ^ Show help
  }
  deriving (Show)

{-| Default CLI arguments.
-}
defaultCLIArgs :: CLIArgs
defaultCLIArgs = CLIArgs
  { argFile = Nothing
  , argDir = Nothing
  , argOutputFormat = Text
  , argGenerateHS = False
  , argStrict = False
  , argHelp = False
  }

-- ============================================================================
-- ARGUMENT PARSING
-- ============================================================================

{-| Parse command-line arguments.
Deterministic: Same arguments always produce same CLIArgs.
-}
parseCLIArgs :: [String] -> Either String CLIArgs
parseCLIArgs args = go args defaultCLIArgs
  where
    go :: [String] -> CLIArgs -> Either String CLIArgs
    go [] acc = Right acc
    go ("--file":path:rest) acc = go rest (acc { argFile = Just path })
    go ("--dir":path:rest) acc = go rest (acc { argDir = Just path })
    go ("--output":"json":rest) acc = go rest (acc { argOutputFormat = JSON })
    go ("--output":"text":rest) acc = go rest (acc { argOutputFormat = Text })
    go ("--output":"csv":rest) acc = go rest (acc { argOutputFormat = CSV })
    go ("--generate-haskell":rest) acc = go rest (acc { argGenerateHS = True })
    go ("--strict":rest) acc = go rest (acc { argStrict = True })
    go ("--help":rest) acc = go rest (acc { argHelp = True })
    go ("-h":rest) acc = go rest (acc { argHelp = True })
    go (unknown:_) _ = Left $ "Unknown argument: " ++ unknown

-- ============================================================================
-- MAIN CLI ENTRY POINT
-- ============================================================================

{-| Main CLI entry point.
Parse arguments and dispatch to appropriate action.
-}
main :: IO ()
main = do
  args <- getArgs
  case parseCLIArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      exitFailure
    Right opts
      | argHelp opts -> showHelp
      | otherwise -> runCLI opts

{-| Run CLI with parsed arguments.
-}
runCLI :: CLIArgs -> IO ()
runCLI opts
  | argHelp opts = showHelp
  | maybe False (not . null) (argFile opts) = do
      let file = case argFile opts of
                   Just f -> f
                   Nothing -> error "File not provided"
      report <- verifyFile file
      case argOutputFormat opts of
        JSON -> putStrLn (toJSON report)
        Text -> putStrLn (toHumanReadable report)
        CSV -> putStrLn (toCSV report)
      let exitCode = if vrVerdict report == Reject then ExitFailure 1 else ExitSuccess
      exitWith exitCode
  | maybe False (not . null) (argDir opts) = do
      let dir = case argDir opts of
                  Just d -> d
                  Nothing -> error "Directory not provided"
      batchReport <- ciVerify dir
      case argOutputFormat opts of
        JSON -> putStrLn (generateBatchJSON batchReport)
        Text -> putStrLn (generateBatchHumanReadable batchReport)
        CSV -> putStrLn (generateBatchCSV batchReport)
      exitWith (getExitCode batchReport)
  | otherwise = do
      hPutStrLn stderr "Error: Must provide either --file or --dir"
      exitFailure
  where
    vrVerdict r = Assertica.Verify.Report.vrVerdict r

{-| Show help message.
-}
showHelp :: IO ()
showHelp = do
  progName <- getProgName
  putStrLn $ unlines
    [ "Usage: " ++ progName ++ " [options]"
    , ""
    , "Options:"
    , "  --file FILE            Verify single file"
    , "  --dir DIR              Verify all .as files in directory"
    , "  --output FORMAT        Output format: json, text, csv (default: text)"
    , "  --generate-haskell     Generate Haskell code for verified modules"
    , "  --strict               Treat warnings as errors"
    , "  --help, -h             Show this help message"
    , ""
    , "Examples:"
    , "  " ++ progName ++ " --file example.as"
    , "  " ++ progName ++ " --dir ./proofs --output json"
    , "  " ++ progName ++ " --dir ./proofs --generate-haskell --output text"
    , ""
    , "Exit codes:"
    , "  0                      All verifications passed"
    , "  N > 0                  N verifications failed"
    ]
  exitSuccess
