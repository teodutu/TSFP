{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Evaluation.SubstitutionTest where

import Evaluation.Substitution
import Test.Framework
import Util

test_freeVars :: IO ()
test_freeVars = do
    assertListsEqualAsSets ["x"] $ freeVarsInternal "x"
    assertListsEqualAsSets [] $ freeVarsInternal "\\x.x"
    assertListsEqualAsSets ["y"] $ freeVarsInternal "\\x.y"
    assertListsEqualAsSets ["x", "y"] $ freeVarsInternal "(x y)"
    assertListsEqualAsSets ["x", "y", "z"] $ freeVarsInternal "((x y) z)"
    assertListsEqualAsSets [] $ freeVarsInternal "(\\x.x \\y.y)"
    assertListsEqualAsSets ["x"] $ freeVarsInternal "(\\x.x x)"
    assertListsEqualAsSets [] $ freeVarsInternal "\\x.(x x)"
  where
    freeVarsInternal = freeVars . internal
    
test_subst :: IO ()
test_subst = do
    -- Replace x with z in x
    assertEqual "z" $ substInternal "x" "z" "x"
    assertEqual "y" $ substInternal "x" "z" "y"
    assertEqual "\\x.x" $ substInternal "x" "z" "\\x.x"
    assertEqual "\\y.z" $ substInternal "x" "z" "\\y.x"
    assertEqual "\\y#.y" $ substInternal "x" "y" "\\y.x"
    assertEqual "(y y)" $ substInternal "x" "y" "(x x)"
    assertEqual "\\y#.(y# y)" $ substInternal "x" "y" "\\y.(y x)"
    assertEqual "\\y#.(y \\y#.y)" $ substInternal "x" "y" "\\y.(x \\y.x)"
  where
    substInternal var new expr = show $ subst var (internal new) (internal expr)