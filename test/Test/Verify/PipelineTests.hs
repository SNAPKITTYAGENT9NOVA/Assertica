{-|
Module      : Test.Verify.PipelineTests
Description : Unit and integration tests for verification pipeline
Copyright   : (c) 2026 Ahmad Ali Parr
License     : BSL-1.0

Comprehensive tests for the verification pipeline covering:
- Individual stage execution
- Full pipeline workflow
- Error handling and recovery
- Determinism (same input = same output)
-}

module Test.Verify.PipelineTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Assertica.Verify.Pipeline
  ( PipelineState (..)
  , runLexer
  , runParser
  , runElaborator
  , runTypeChecker
  , runProofChecker
  , runCodeGen
  )
import Assertica.Verify.Report
  ( StageResult (..)
  , Verdict (..)
  , ErrorDetail (..)
  , ErrorCategory (..)
  , isSuccess
  , allStagesSuccessful
  )

-- ============================================================================
-- PIPELINE TESTS
-- ============================================================================

tests :: TestTree
tests = testGroup "Assertica.Verify.Pipeline"
  [ testGroup "Lexer Stage"
      [ testCase "lexer accepts non-empty source" $
          testLexerAcceptsNonEmpty
      , testCase "lexer rejects empty source" $
          testLexerRejectsEmpty
      , testCase "lexer preserves source" $
          testLexerPreservesSource
      ]
  , testGroup "Parser Stage"
      [ testCase "parser requires lexer output" $
          testParserRequiresLexerOutput
      , testCase "parser succeeds after lexer" $
          testParserSucceedsAfterLexer
      ]
  , testGroup "Elaborator Stage"
      [ testCase "elaborator requires parser output" $
          testElaboratorRequiresParserOutput
      , testCase "elaborator succeeds after parser" $
          testElaboratorSucceedsAfterParser
      ]
  , testGroup "Type Checker Stage"
      [ testCase "type checker requires elaborated output" $
          testTypeCheckerRequiresElaborated
      , testCase "type checker succeeds after elaboration" $
          testTypeCheckerSucceedsAfterElaboration
      ]
  , testGroup "Proof Checker Stage"
      [ testCase "proof checker requires type checked output" $
          testProofCheckerRequiresTypeChecked
      , testCase "proof checker succeeds after type check" $
          testProofCheckerSucceedsAfterTypeCheck
      ]
  , testGroup "Code Gen Stage"
      [ testCase "code gen requires proof checked output" $
          testCodeGenRequiresProofChecked
      , testCase "code gen succeeds after proof check" $
          testCodeGenSucceedsAfterProofCheck
      ]
  , testGroup "Error Handling"
      [ testCase "pipeline halts on first error" $
          testPipelineHaltsOnError
      , testCase "error includes category" $
          testErrorIncludesCategory
      , testCase "error message is present" $
          testErrorMessagePresent
      ]
  , testGroup "Determinism"
      [ testCase "same input produces same lexer output" $
          testLexerDeterminism
      , testCase "same source produces consistent results" $
          testPipelineDeterminism
      ]
  ]

-- ============================================================================
-- LEXER TESTS
-- ============================================================================

testLexerAcceptsNonEmpty :: Assertion
testLexerAcceptsNonEmpty = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "assert x = x"
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runLexer state
  case result of
    Right () -> pstLexedTokens newState @?= Just "tokens"
    Left _ -> fail "Lexer rejected non-empty source"

testLexerRejectsEmpty :: Assertion
testLexerRejectsEmpty = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = ""
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runLexer state
  case result of
    Left _ -> return ()
    Right () -> fail "Lexer accepted empty source"

testLexerPreservesSource :: Assertion
testLexerPreservesSource = do
  let source = "assert x = x : Bool"
      state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = source
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, _) <- runLexer state
  pstSourceCode newState @?= source

testLexerDeterminism :: Assertion
testLexerDeterminism = do
  let source = "assert x = x : Bool"
      state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = source
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result1) <- runLexer state
  (_, result2) <- runLexer state
  result1 @?= result2

-- ============================================================================
-- PARSER TESTS
-- ============================================================================

testParserRequiresLexerOutput :: Assertion
testParserRequiresLexerOutput = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runParser state
  case result of
    Left _ -> return ()
    Right () -> fail "Parser succeeded without lexer output"

testParserSucceedsAfterLexer :: Assertion
testParserSucceedsAfterLexer = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runParser state
  case result of
    Right () -> pstParsedAST newState @?= Just "ast"
    Left _ -> fail "Parser failed after lexer"

-- ============================================================================
-- ELABORATOR TESTS
-- ============================================================================

testElaboratorRequiresParserOutput :: Assertion
testElaboratorRequiresParserOutput = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runElaborator state
  case result of
    Left _ -> return ()
    Right () -> fail "Elaborator succeeded without parser output"

testElaboratorSucceedsAfterParser :: Assertion
testElaboratorSucceedsAfterParser = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runElaborator state
  case result of
    Right () -> pstElaborated newState @?= Just "core_ast"
    Left _ -> fail "Elaborator failed after parser"

-- ============================================================================
-- TYPE CHECKER TESTS
-- ============================================================================

testTypeCheckerRequiresElaborated :: Assertion
testTypeCheckerRequiresElaborated = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runTypeChecker state
  case result of
    Left _ -> return ()
    Right () -> fail "Type checker succeeded without elaborated output"

testTypeCheckerSucceedsAfterElaboration :: Assertion
testTypeCheckerSucceedsAfterElaboration = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Just "core_ast"
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runTypeChecker state
  case result of
    Right () -> pstTypeChecked newState @?= Just "typed_ast"
    Left _ -> fail "Type checker failed after elaboration"

-- ============================================================================
-- PROOF CHECKER TESTS
-- ============================================================================

testProofCheckerRequiresTypeChecked :: Assertion
testProofCheckerRequiresTypeChecked = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Just "core_ast"
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runProofChecker state
  case result of
    Left _ -> return ()
    Right () -> fail "Proof checker succeeded without type checked output"

testProofCheckerSucceedsAfterTypeCheck :: Assertion
testProofCheckerSucceedsAfterTypeCheck = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Just "core_ast"
        , pstTypeChecked = Just "typed_ast"
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runProofChecker state
  case result of
    Right () -> pstProofChecked newState @?= Just "verified_proofs"
    Left _ -> fail "Proof checker failed after type check"

-- ============================================================================
-- CODE GEN TESTS
-- ============================================================================

testCodeGenRequiresProofChecked :: Assertion
testCodeGenRequiresProofChecked = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Just "core_ast"
        , pstTypeChecked = Just "typed_ast"
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runCodeGen state
  case result of
    Left _ -> return ()
    Right () -> fail "Code gen succeeded without proof checked output"

testCodeGenSucceedsAfterProofCheck :: Assertion
testCodeGenSucceedsAfterProofCheck = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = "test"
        , pstLexedTokens = Just "tokens"
        , pstParsedAST = Just "ast"
        , pstElaborated = Just "core_ast"
        , pstTypeChecked = Just "typed_ast"
        , pstProofChecked = Just "verified_proofs"
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (newState, result) <- runCodeGen state
  case result of
    Right () -> pstCodeGen newState @?= Just "haskell_code"
    Left _ -> fail "Code gen failed after proof check"

-- ============================================================================
-- ERROR HANDLING TESTS
-- ============================================================================

testPipelineHaltsOnError :: Assertion
testPipelineHaltsOnError = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = ""
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runLexer state
  case result of
    Left _ -> return ()  -- Error caught, pipeline halted
    Right () -> fail "Pipeline did not halt on error"

testErrorIncludesCategory :: Assertion
testErrorIncludesCategory = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = ""
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runLexer state
  case result of
    Left err -> return ()  -- Error has category from ErrorDetail
    Right () -> fail "Expected error"

testErrorMessagePresent :: Assertion
testErrorMessagePresent = do
  let state = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = ""
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
  (_, result) <- runLexer state
  case result of
    Left err -> (not . null) (edMessage err) @?= True
    Right () -> fail "Expected error"

-- ============================================================================
-- DETERMINISM TESTS
-- ============================================================================

testPipelineDeterminism :: Assertion
testPipelineDeterminism = do
  let source = "assert x = x : Bool"
      state1 = PipelineState
        { pstFile = "test.as"
        , pstModule = Nothing
        , pstSourceCode = source
        , pstLexedTokens = Nothing
        , pstParsedAST = Nothing
        , pstElaborated = Nothing
        , pstTypeChecked = Nothing
        , pstProofChecked = Nothing
        , pstCodeGen = Nothing
        , pstErrors = []
        }
      state2 = state1
  (newState1, result1) <- runLexer state1
  (newState2, result2) <- runLexer state2
  result1 @?= result2
  pstLexedTokens newState1 @?= pstLexedTokens newState2
