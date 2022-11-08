{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Evaluation.NormalTest where

import Evaluation.Normal
import Test.Framework
import Control.Arrow (first)
import Util

test_eval :: IO ()
test_eval = do
    -- EvalVar: undefined variable
    assertEqual ("x", empty) $ evalInternal "x" empty
    -- EvalVar: defined variable
    let context = makeContext [("x", "y")] in
        assertEqual ("y", context) $ evalInternal "x" context
    -- Lambda
    assertEqual ("\\x.x", empty) $ evalInternal "\\x.x" empty
    -- Reduce
    assertEqual ("y", empty) $ evalInternal "(\\x.x y)" empty
    -- EvalApp
    let context = makeContext [("id", "\\x.x")] in
        assertEqual ("(\\x.x y)", context) $ evalInternal "(id y)" context
    -- Lambda
    assertEqual ("\\x.(\\x.x x)", empty) $
                evalInternal "\\x.(\\x.x x)" empty
    -- Definition
    let (_, context) = evalInternal "id=\\x.x" empty in
        assertEqual (makeContext [("id", "\\x.x")]) context
    -- Something specific to normal-order, as opposed to applicative-order
    assertEqual ("y", empty) $
                evalInternal "(\\x.y (\\x.(x x) \\x.(x x)))" empty
  where
    evalInternal expr context = first show $ eval (internal expr) context
    empty = makeContext []