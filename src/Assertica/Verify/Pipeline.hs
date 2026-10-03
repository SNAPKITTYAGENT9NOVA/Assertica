{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Assertica.Verify.Pipeline
Description : Main verification pipeline orchestrator
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

The verification pipeline orchestrates the complete compilation and verification workflow:

  SOURCE → LEXER → PARSER → ELABORATOR → TYPE CHECKER → PROOF CHECKER → CODE GEN → ACCEPT/REJECT

Each stage:
- Operates on the output of the previous stage
- Tracks execution time
- Captures errors with source locations
- Halts immediately on failure (fail-closed)

DESIGN PRINCIPLES:
1. Deterministic: Same input always produces same report
2. Fail-closed: First error halts pipeline
3. Observable: Each stage tracked independently
4. Composable: Results can be tested in isolation
-}

module Assertica.Verify.Pipeline
  ( -- * Main pipeline entry points
    verifyFile
  , verifyDirectory
  , verifyModule

    -- * Pipeline stages
  , PipelineState (..)
  , runPipelineStage
  , runLexer
  , runParser
  , runElaborator
  , runTypeChecker
  , runProofChecker
  , runCodeGen

    -- * Error handling
  , toPipelineError
  , PipelineError (..)
  ) where

import Control.Monad (filterM)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Data.List (sort)
import Assertica.Verify.Report
  ( VerificationReport (..)
  , StageResult
  , Verdict (..)
  , ErrorDetail (..)
  , ErrorCategory (..)
  , SourceLocation (..)
  , mkStageResult
  , mkFailedStageResult
  , mkReport
  )

-- ============================================================================
-- PIPELINE STATE
-- ============================================================================

{-| State of the verification pipeline for a single file.
-}
data PipelineState = PipelineState
  { pstFile         :: FilePath
  , pstModule       :: Maybe String
  , pstSourceCode   :: String              -- ^ Original source
  , pstLexedTokens  :: Maybe String        -- ^ Lexer output
  , pstParsedAST    :: Maybe String        -- ^ Parser output
  , pstElaborated   :: Maybe String        -- ^ Elaborator output
  , pstTypeChecked  :: Maybe String        -- ^ Type checker output
  , pstProofChecked :: Maybe String        -- ^ Proof checker output
  , pstCodeGen      :: Maybe String        -- ^ Code generation output
  , pstErrors       :: [ErrorDetail]       -- ^ Accumulated errors
  }
  deriving (Show)

-- ============================================================================
-- PIPELINE ERRORS
-- ============================================================================

{-| Pipeline-specific error type.
-}
data PipelineError
  = PipelineError String (Maybe SourceLocation)
  | FileNotFound FilePath
  | DirectoryNotFound FilePath
  | InvalidExtension FilePath
  deriving (Show)

{-| Convert pipeline error to ErrorDetail.
-}
toPipelineError :: PipelineError -> ErrorDetail
toPipelineError (PipelineError msg loc) =
  ErrorDetail UnknownError msg loc Nothing Nothing
toPipelineError (FileNotFound path) =
  ErrorDetail UnknownError ("File not found: " ++ path) Nothing Nothing (Just "Check file path")
toPipelineError (DirectoryNotFound path) =
  ErrorDetail UnknownError ("Directory not found: " ++ path) Nothing Nothing (Just "Check directory path")
toPipelineError (InvalidExtension path) =
  ErrorDetail SyntaxError ("Invalid file extension: " ++ path) Nothing Nothing (Just "Use .as file extension")

-- ============================================================================
-- MAIN PIPELINE ENTRY POINTS
-- ============================================================================

{-| Verify a single .as file and return comprehensive report.
-}
verifyFile :: FilePath -> IO VerificationReport
verifyFile filePath = do
  start <- getCurrentTime

  -- Check file exists
  exists <- doesFileExist filePath
  if not exists
    then do
      end <- getCurrentTime
      let duration = realToFrac (diffUTCTime end start) * 1000
          err = toPipelineError (FileNotFound filePath)
          failStage = mkFailedStageResult "Read" 0 err
      return $ mkReport filePath Nothing Nothing (show start) failStage failStage failStage failStage failStage
    else do
      -- Check extension
      if takeExtension filePath /= ".as"
        then do
          end <- getCurrentTime
          let duration = realToFrac (diffUTCTime end start) * 1000
              err = toPipelineError (InvalidExtension filePath)
              failStage = mkFailedStageResult "Read" 0 err
          return $ mkReport filePath Nothing Nothing (show start) failStage failStage failStage failStage failStage
        else do
          -- Read source
          sourceCode <- readFile filePath

          -- Initialize pipeline state
          let initialState = PipelineState
                { pstFile = filePath
                , pstModule = Nothing
                , pstSourceCode = sourceCode
                , pstLexedTokens = Nothing
                , pstParsedAST = Nothing
                , pstElaborated = Nothing
                , pstTypeChecked = Nothing
                , pstProofChecked = Nothing
                , pstCodeGen = Nothing
                , pstErrors = []
                }

          -- Run pipeline stages
          result <- runPipeline filePath (show start) initialState

          end <- getCurrentTime
          let totalDuration = realToFrac (diffUTCTime end start) * 1000

          return $ result { vrTotalDuration = totalDuration }

{-| Verify all .as files in a directory.
-}
verifyDirectory :: FilePath -> IO [VerificationReport]
verifyDirectory dirPath = do
  exists <- doesDirectoryExist dirPath
  if not exists
    then do
      let err = toPipelineError (DirectoryNotFound dirPath)
          failStage = mkFailedStageResult "Read" 0 err
          report = mkReport dirPath Nothing Nothing "unknown" failStage failStage failStage failStage failStage
      return [report]
    else do
      files <- listDirectory dirPath
      let asserticaFiles = filter (\f -> takeExtension f == ".as") files
      let fullPaths = map (dirPath </>) (sort asserticaFiles)
      mapM verifyFile fullPaths

{-| Verify a single module (used internally after elaboration).
-}
verifyModule :: String -> String -> IO VerificationReport
verifyModule moduleName sourceCode = do
  -- Placeholder: verify a module given its name and source
  -- This would be used for internal verification of elaborated modules
  start <- getCurrentTime
  let failStage = mkStageResult "Module" 0
  return $ mkReport moduleName (Just moduleName) Nothing (show start) failStage failStage failStage failStage failStage

-- ============================================================================
-- PIPELINE ORCHESTRATION
-- ============================================================================

{-| Run the complete verification pipeline.
Halts on first error (fail-closed).
-}
runPipeline :: FilePath -> String -> PipelineState -> IO VerificationReport
runPipeline filePath timestamp state = do
  start <- getCurrentTime

  -- Stage 1: Lexer
  lexStart <- getCurrentTime
  (lexState, lexResult) <- runLexer state
  lexEnd <- getCurrentTime
  let lexDuration = realToFrac (diffUTCTime lexEnd lexStart) * 1000

  case lexResult of
    Left err -> do
      let failStage = mkFailedStageResult "Lexer" lexDuration err
      return $ mkReport filePath Nothing Nothing timestamp failStage failStage failStage failStage failStage
    Right _ -> do
      -- Stage 2: Parser
      parStart <- getCurrentTime
      (parState, parResult) <- runParser lexState
      parEnd <- getCurrentTime
      let parDuration = realToFrac (diffUTCTime parEnd parStart) * 1000

      case parResult of
        Left err -> do
          let lexStage = mkStageResult "Lexer" lexDuration
              failStage = mkFailedStageResult "Parser" parDuration err
          return $ mkReport filePath Nothing Nothing timestamp lexStage failStage failStage failStage failStage
        Right _ -> do
          -- Stage 3: Elaborator
          eStart <- getCurrentTime
          (eState, eResult) <- runElaborator parState
          eEnd <- getCurrentTime
          let eDuration = realToFrac (diffUTCTime eEnd eStart) * 1000

          case eResult of
            Left err -> do
              let lexStage = mkStageResult "Lexer" lexDuration
                  parStage = mkStageResult "Parser" parDuration
                  failStage = mkFailedStageResult "Elaborator" eDuration err
              return $ mkReport filePath Nothing Nothing timestamp lexStage parStage failStage failStage failStage
            Right _ -> do
              -- Stage 4: Type Checker
              tcStart <- getCurrentTime
              (tcState, tcResult) <- runTypeChecker eState
              tcEnd <- getCurrentTime
              let tcDuration = realToFrac (diffUTCTime tcEnd tcStart) * 1000

              case tcResult of
                Left err -> do
                  let lexStage = mkStageResult "Lexer" lexDuration
                      parStage = mkStageResult "Parser" parDuration
                      eStage = mkStageResult "Elaborator" eDuration
                      failStage = mkFailedStageResult "TypeChecker" tcDuration err
                  return $ mkReport filePath (pstModule tcState) Nothing timestamp lexStage parStage eStage failStage failStage
                Right _ -> do
                  -- Stage 5: Proof Checker
                  pcStart <- getCurrentTime
                  (pcState, pcResult) <- runProofChecker tcState
                  pcEnd <- getCurrentTime
                  let pcDuration = realToFrac (diffUTCTime pcEnd pcStart) * 1000

                  case pcResult of
                    Left err -> do
                      let lexStage = mkStageResult "Lexer" lexDuration
                          parStage = mkStageResult "Parser" parDuration
                          eStage = mkStageResult "Elaborator" eDuration
                          tcStage = mkStageResult "TypeChecker" tcDuration
                          failStage = mkFailedStageResult "ProofChecker" pcDuration err
                      return $ mkReport filePath (pstModule pcState) Nothing timestamp lexStage parStage eStage tcStage failStage
                    Right _ -> do
                      -- Stage 6: Code Generation
                      cgStart <- getCurrentTime
                      (cgState, cgResult) <- runCodeGen pcState
                      cgEnd <- getCurrentTime
                      let cgDuration = realToFrac (diffUTCTime cgEnd cgStart) * 1000

                      case cgResult of
                        Left err -> do
                          let lexStage = mkStageResult "Lexer" lexDuration
                              parStage = mkStageResult "Parser" parDuration
                              eStage = mkStageResult "Elaborator" eDuration
                              tcStage = mkStageResult "TypeChecker" tcDuration
                              pcStage = mkStageResult "ProofChecker" pcDuration
                              failStage = mkFailedStageResult "CodeGen" cgDuration err
                          return $ mkReport filePath (pstModule cgState) Nothing timestamp lexStage parStage eStage tcStage failStage
                        Right _ -> do
                          let lexStage = mkStageResult "Lexer" lexDuration
                              parStage = mkStageResult "Parser" parDuration
                              eStage = mkStageResult "Elaborator" eDuration
                              tcStage = mkStageResult "TypeChecker" tcDuration
                              pcStage = mkStageResult "ProofChecker" pcDuration
                              cgStage = mkStageResult "CodeGen" cgDuration
                          return $ mkReport filePath (pstModule cgState) Nothing timestamp lexStage parStage eStage tcStage cgStage

-- ============================================================================
-- INDIVIDUAL PIPELINE STAGES
-- ============================================================================

{-| Run lexer stage (tokenization).
-}
runLexer :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runLexer state = do
  -- Placeholder: real implementation would call Assertica.Surface.Lexer
  -- For now, we simulate successful lexing
  if null (pstSourceCode state)
    then do
      let err = ErrorDetail SyntaxError "Empty source code" Nothing Nothing Nothing
      return (state, Left err)
    else do
      let state' = state { pstLexedTokens = Just "tokens" }
      return (state', Right ())

{-| Run parser stage (build AST).
-}
runParser :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runParser state = do
  -- Placeholder: real implementation would call Assertica.Surface.Parser
  case pstLexedTokens state of
    Nothing ->
      let err = ErrorDetail SyntaxError "Lexer output missing" Nothing Nothing Nothing
      in return (state, Left err)
    Just _ ->
      let state' = state { pstParsedAST = Just "ast" }
      in return (state', Right ())

{-| Run elaborator stage (surface to core AST).
-}
runElaborator :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runElaborator state = do
  -- Placeholder: real implementation would call Assertica.Surface.Elaborator
  case pstParsedAST state of
    Nothing ->
      let err = ErrorDetail SyntaxError "Parser output missing" Nothing Nothing Nothing
      in return (state, Left err)
    Just _ ->
      let state' = state { pstElaborated = Just "core_ast" }
      in return (state', Right ())

{-| Run type checker stage.
-}
runTypeChecker :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runTypeChecker state = do
  -- Placeholder: real implementation would call Assertica.Core.TypeChecker
  case pstElaborated state of
    Nothing ->
      let err = ErrorDetail TypeError "Elaborated AST missing" Nothing Nothing Nothing
      in return (state, Left err)
    Just _ ->
      let state' = state { pstTypeChecked = Just "typed_ast" }
      in return (state', Right ())

{-| Run proof checker stage.
-}
runProofChecker :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runProofChecker state = do
  -- Placeholder: real implementation would call Agent 2B's proof checker
  case pstTypeChecked state of
    Nothing ->
      let err = ErrorDetail ProofObligation "Type checked AST missing" Nothing Nothing Nothing
      in return (state, Left err)
    Just _ ->
      let state' = state { pstProofChecked = Just "verified_proofs" }
      in return (state', Right ())

{-| Run code generation stage.
-}
runCodeGen :: PipelineState -> IO (PipelineState, Either ErrorDetail ())
runCodeGen state = do
  -- Placeholder: real implementation would call Agent 6A's code generator
  case pstProofChecked state of
    Nothing ->
      let err = ErrorDetail CodeGenError "Proof checked AST missing" Nothing Nothing Nothing
      in return (state, Left err)
    Just _ ->
      let state' = state { pstCodeGen = Just "haskell_code" }
      in return (state', Right ())

-- ============================================================================
-- PIPELINE UTILITY
-- ============================================================================

{-| Run a single pipeline stage with timing and error handling.
-}
runPipelineStage
  :: String
  -> (PipelineState -> IO (PipelineState, Either ErrorDetail ()))
  -> PipelineState
  -> IO (PipelineState, StageResult, Double)
runPipelineStage stageName stageFunc state = do
  start <- getCurrentTime
  (state', result) <- stageFunc state
  end <- getCurrentTime
  let duration = realToFrac (diffUTCTime end start) * 1000
  case result of
    Right () -> return (state', mkStageResult stageName duration, duration)
    Left err -> return (state', mkFailedStageResult stageName duration err, duration)
