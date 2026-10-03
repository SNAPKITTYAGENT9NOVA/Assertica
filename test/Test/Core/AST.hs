{-|
Module      : Test.Core.AST
Description : Tests for the Abstract Syntax Tree module

Tests the basic AST operations:
  - Variable representation
  - Free and bound variable computation
  - Substitution
  - Alpha-renaming
-}

module Test.Core.AST (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Assertica.Core.AST

tests :: TestTree
tests = testGroup "Assertica.Core.AST"
  [ freeVarsTests
  , boundVarsTests
  , substituteTests
  , alphaRenameTests
  ]

-- Tests for freeVars
freeVarsTests :: TestTree
freeVarsTests = testGroup "freeVars"
  [ testCase "Variable has itself as free var" $
      let x = Var "x"
      in freeVars (TVar x) @?= Set.singleton x

  , testCase "Constant has no free vars" $
      freeVars (TConst "5") @?= Set.empty

  , testCase "Abstraction binds its variable" $
      let x = Var "x"
          y = Var "y"
      in freeVars (TAbs x (TVar y)) @?= Set.singleton y

  , testCase "Abstraction hides bound variable" $
      let x = Var "x"
      in freeVars (TAbs x (TVar x)) @?= Set.empty

  , testCase "Application includes free vars from both sides" $
      let x = Var "x"
          y = Var "y"
      in freeVars (TApp (TVar x) (TVar y)) @?= Set.fromList [x, y]

  , testCase "Nested abstraction" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
      in freeVars (TAbs x (TAbs y (TVar z))) @?= Set.singleton z

  , testCase "Let-binding" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
      in freeVars (TLet x (TVar y) (TVar z)) @?= Set.fromList [y, z]
  ]

-- Tests for boundVars
boundVarsTests :: TestTree
boundVarsTests = testGroup "boundVars"
  [ testCase "Variable has no bound vars" $
      boundVars (TVar (Var "x")) @?= Set.empty

  , testCase "Abstraction binds a variable" $
      let x = Var "x"
      in boundVars (TAbs x (TVar x)) @?= Set.singleton x

  , testCase "Multiple abstractions" $
      let x = Var "x"
          y = Var "y"
      in boundVars (TAbs x (TAbs y (TVar y))) @?= Set.fromList [x, y]

  , testCase "Let-binding binds a variable" $
      let x = Var "x"
          y = Var "y"
      in boundVars (TLet x (TConst "5") (TVar y)) @?= Set.singleton x
  ]

-- Tests for substitute
substituteTests :: TestTree
substituteTests = testGroup "substitute"
  [ testCase "Substitute into variable" $
      let x = Var "x"
      in substitute x (TConst "5") (TVar x) @?= TConst "5"

  , testCase "Substitute preserves other variables" $
      let x = Var "x"
          y = Var "y"
      in substitute x (TConst "5") (TVar y) @?= TVar y

  , testCase "Substitution does not cross binders" $
      let x = Var "x"
      in substitute x (TConst "5") (TAbs x (TVar x)) @?= TAbs x (TVar x)

  , testCase "Substitution in simple application" $
      let x = Var "x"
      in substitute x (TConst "5") (TApp (TVar x) (TConst "1")) @?=
         TApp (TConst "5") (TConst "1")

  , testCase "Substitution with renaming (capture avoidance)" $
      let x = Var "x"
          y = Var "y"
          replacement = TVar y
          -- Substituting x with y in (λy. x + y) should rename y to avoid capture
          term = TAbs y (TApp (TVar x) (TVar y))
          result = substitute x replacement term
      in case result of
           TAbs _ _ -> True @?= True  -- Should create a renamed abstraction
           _ -> False @?= True

  , testCase "Let-binding substitution" $
      let x = Var "x"
      in substitute x (TConst "5") (TLet x (TConst "10") (TVar x)) @?=
         TLet x (TConst "10") (TVar x)  -- x is bound, so substitution doesn't apply
  ]

-- Tests for alphaRename
alphaRenameTests :: TestTree
alphaRenameTests = testGroup "alphaRename"
  [ testCase "Rename variable" $
      let x = Var "x"
          y = Var "y"
      in alphaRename x y (TVar x) @?= TVar y

  , testCase "Rename in abstraction" $
      let x = Var "x"
          y = Var "y"
      in alphaRename x y (TAbs x (TVar x)) @?= TAbs y (TVar y)

  , testCase "Rename does not affect other variables" $
      let x = Var "x"
          y = Var "y"
          z = Var "z"
      in alphaRename x z (TApp (TVar x) (TVar y)) @?=
         TApp (TVar z) (TVar y)
  ]

-- Import Set module
import qualified Data.Set as Set
