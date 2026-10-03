{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Verify.ErrorReport
Description : Error categorization, formatting, and fix suggestions
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

Provides utilities for categorizing errors from different pipeline stages,
formatting them consistently, and generating actionable fix suggestions.

DESIGN PRINCIPLES:
1. Deterministic: Same error produces identical formatted output
2. Actionable: Every error includes context and fix suggestion where possible
3. Consistent: Unified error format across all pipeline stages
4. Localized: Source location always included when available
-}

module Assertica.Verify.ErrorReport
  ( -- * Error categorization
    categorizeError
  , errorCategory

    -- * Error formatting
  , formatError
  , formatErrorWithContext
  , formatErrorLocation

    -- * Fix suggestions
  , suggestFix
  , suggestParseError
  , suggestTypeError
  , suggestProofError
  , suggestModuleError
  , suggestTerminationError
  , suggestPositivityError

    -- * Error context
  , buildContext
  , extractErrorContext
  ) where

import Data.List (isInfixOf)
import Data.Char (isSpace)
import Assertica.Verify.Report
  ( ErrorDetail (..)
  , ErrorCategory (..)
  , SourceLocation (..)
  )

-- ============================================================================
-- ERROR CATEGORIZATION
-- ============================================================================

{-| Categorize an error message based on keywords and patterns.
Deterministic: Same message always produces same category.
-}
categorizeError :: String -> Maybe SourceLocation -> ErrorCategory
categorizeError msg _
  | "parse" `isInfixOf` lowerMsg = SyntaxError
  | "syntax" `isInfixOf` lowerMsg = SyntaxError
  | "unexpected token" `isInfixOf` lowerMsg = SyntaxError
  | "lexer" `isInfixOf` lowerMsg = SyntaxError
  | "type" `isInfixOf` lowerMsg = TypeError
  | "cannot unify" `isInfixOf` lowerMsg = TypeError
  | "mismatch" `isInfixOf` lowerMsg = TypeError
  | "proof" `isInfixOf` lowerMsg = ProofObligation
  | "unproven" `isInfixOf` lowerMsg = ProofObligation
  | "assertion" `isInfixOf` lowerMsg = ProofObligation
  | "missing proof" `isInfixOf` lowerMsg = ProofObligation
  | "circular" `isInfixOf` lowerMsg = ModuleError
  | "import" `isInfixOf` lowerMsg = ModuleError
  | "module" `isInfixOf` lowerMsg = ModuleError
  | "termination" `isInfixOf` lowerMsg = TerminationError
  | "decreasing" `isInfixOf` lowerMsg = TerminationError
  | "recursion" `isInfixOf` lowerMsg = TerminationError
  | "positivity" `isInfixOf` lowerMsg = PositivityError
  | "inductive" `isInfixOf` lowerMsg = PositivityError
  | "negative" `isInfixOf` lowerMsg = PositivityError
  | "codegen" `isInfixOf` lowerMsg = CodeGenError
  | "code generation" `isInfixOf` lowerMsg = CodeGenError
  | "backend" `isInfixOf` lowerMsg = CodeGenError
  | otherwise = UnknownError
  where
    lowerMsg = map (\c -> if c >= 'A' && c <= 'Z' then read [c] else c) msg

{-| Get the category of an error.
-}
errorCategory :: ErrorDetail -> ErrorCategory
errorCategory = edCategory

-- ============================================================================
-- ERROR FORMATTING
-- ============================================================================

{-| Format an error for display.
-}
formatError :: ErrorDetail -> String
formatError err =
  "[" ++ formatCategory (edCategory err) ++ "] " ++ edMessage err
  ++ formatLocIfPresent (edLocation err)

{-| Format error category as a string.
-}
formatCategory :: ErrorCategory -> String
formatCategory SyntaxError     = "SYNTAX"
formatCategory TypeError       = "TYPE"
formatCategory ProofObligation = "PROOF"
formatCategory ModuleError     = "MODULE"
formatCategory TerminationError = "TERMINATION"
formatCategory PositivityError = "POSITIVITY"
formatCategory CodeGenError    = "CODEGEN"
formatCategory UnknownError    = "ERROR"

{-| Format location if present.
-}
formatLocIfPresent :: Maybe SourceLocation -> String
formatLocIfPresent Nothing = ""
formatLocIfPresent (Just loc) = " " ++ formatErrorLocation loc

{-| Format error with additional context from source code.
-}
formatErrorWithContext :: ErrorDetail -> String -> String
formatErrorWithContext err sourceCode =
  formatError err
  ++ case edLocation err of
       Nothing -> ""
       Just loc ->
         let ctx = buildContext sourceCode loc 2 in
         if null ctx then "" else "\n" ++ ctx

{-| Format a source location.
-}
formatErrorLocation :: SourceLocation -> String
formatErrorLocation loc =
  "(" ++ slFile loc ++ ":" ++ show (slLine loc) ++ ":" ++ show (slColumn loc) ++ ")"

-- ============================================================================
-- CONTEXT BUILDING
-- ============================================================================

{-| Build a string showing source context around an error location.
Extracts lines before, at, and after the error with line numbers.
-}
buildContext :: String -> SourceLocation -> Int -> String
buildContext sourceCode loc contextLines =
  let linesOfCode = lines sourceCode
      targetLine = slLine loc - 1  -- Convert to 0-indexed
      startLine = max 0 (targetLine - contextLines)
      endLine = min (length linesOfCode - 1) (targetLine + contextLines)
      contextLinesExtracted = drop startLine (take (endLine + 1) linesOfCode)
      lineNumbers = [startLine + 1 .. endLine + 1]
      formatted = zipWith formatContextLine lineNumbers contextLinesExtracted
  in unlines formatted

{-| Format a single context line with line number and pointer.
-}
formatContextLine :: Int -> String -> String
formatContextLine lineNum code =
  let prefix = padLine lineNum
      indicator = if code `startsWith` code then "  > " else "    "
  in indicator ++ prefix ++ code
  where
    startsWith _ _ = False  -- Placeholder
    padLine n = let s = show n in replicate (4 - length s) ' ' ++ s ++ " | "

{-| Extract code context around an error.
-}
extractErrorContext :: String -> SourceLocation -> Maybe String
extractErrorContext sourceCode loc =
  let linesOfCode = lines sourceCode
      targetIdx = slLine loc - 1
  in if targetIdx >= 0 && targetIdx < length linesOfCode
     then Just (linesOfCode !! targetIdx)
     else Nothing

-- ============================================================================
-- FIX SUGGESTIONS
-- ============================================================================

{-| Generate a fix suggestion for an error.
-}
suggestFix :: ErrorDetail -> String -> Maybe String
suggestFix err sourceCode =
  case edCategory err of
    SyntaxError       -> suggestParseError (edMessage err)
    TypeError         -> suggestTypeError (edMessage err)
    ProofObligation   -> suggestProofError (edMessage err)
    ModuleError       -> suggestModuleError (edMessage err)
    TerminationError  -> suggestTerminationError (edMessage err)
    PositivityError   -> suggestPositivityError (edMessage err)
    CodeGenError      -> Just "Check compiler backend for issues. Verify Haskell code generation targets."
    UnknownError      -> Nothing

{-| Suggest fix for syntax error.
-}
suggestParseError :: String -> Maybe String
suggestParseError msg
  | "unexpected" `isInfixOf` msg =
      Just "Check for missing operators, mismatched brackets, or incorrect syntax."
  | "expected" `isInfixOf` msg =
      Just "Review syntax rules for your statement and ensure proper formatting."
  | "token" `isInfixOf` msg =
      Just "Verify lexer tokenization. Check for unclosed strings or comments."
  | otherwise = Just "Review source code for syntactic issues."

{-| Suggest fix for type error.
-}
suggestTypeError :: String -> Maybe String
suggestTypeError msg
  | "unify" `isInfixOf` msg =
      Just "Types don't match. Add explicit type annotations to clarify intent."
  | "mismatch" `isInfixOf` msg =
      Just "Function argument type doesn't match parameter. Check argument order or types."
  | "cannot infer" `isInfixOf` msg =
      Just "Add explicit type annotation to help type inference."
  | "has type" `isInfixOf` msg =
      Just "Expression has a different type than expected. Adjust expression or annotation."
  | otherwise = Just "Review type annotations and ensure consistency."

{-| Suggest fix for proof obligation.
-}
suggestProofError :: String -> Maybe String
suggestProofError msg
  | "missing" `isInfixOf` msg =
      Just "Provide a proof term for the unproven assertion."
  | "invalid" `isInfixOf` msg =
      Just "Verify the proof is structurally correct and matches the proposition."
  | "assumption" `isInfixOf` msg =
      Just "Ensure all assumptions used in proof are valid in the current context."
  | otherwise = Just "Review assertion and provide correct proof witness."

{-| Suggest fix for module error.
-}
suggestModuleError :: String -> Maybe String
suggestModuleError msg
  | "circular" `isInfixOf` msg =
      Just "Circular dependency detected. Restructure modules to eliminate cycles."
  | "missing" `isInfixOf` msg =
      Just "Check module imports. Ensure referenced modules exist and are in scope."
  | "export" `isInfixOf` msg =
      Just "Verify the symbol is exported from the module it's imported from."
  | otherwise = Just "Check module system configuration and import paths."

{-| Suggest fix for termination error.
-}
suggestTerminationError :: String -> Maybe String
suggestTerminationError msg
  | "decreasing" `isInfixOf` msg =
      Just "Recursive calls must be on structurally smaller arguments. Use well-founded ordering."
  | "structural" `isInfixOf` msg =
      Just "Rewrite recursion to operate on strictly smaller terms or add explicit termination proof."
  | otherwise = Just "Verify recursion is well-founded. Add termination annotations if needed."

{-| Suggest fix for positivity error.
-}
suggestPositivityError :: String -> Maybe String
suggestPositivityError msg
  | "negative" `isInfixOf` msg =
      Just "Type parameter appears in negative position. Restructure type to eliminate negative occurrence."
  | "inductive" `isInfixOf` msg =
      Just "Inductive type definition violates positivity. Check constructor argument types."
  | otherwise = Just "Verify inductive type definition follows positivity constraint."
