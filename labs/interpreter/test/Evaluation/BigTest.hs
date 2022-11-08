{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Evaluation.BigTest where

import Evaluation.Big
import qualified Evaluation.Normal as EN
import qualified Evaluation.Applicative as EA
import Util
import Syntax.Grammar (parseProgram)
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Test.Framework

test_evalBig :: IO ()
test_evalBig = do
    assertEqual ("x", empty) $ evalBigInternal EN.eval "x" empty
    assertEqual ("x", empty) $ evalBigInternal EA.eval "x" empty
    let context = makeContext [("x", "y"), ("y", "z")] in do
        assertEqual ("z", context) $ evalBigInternal EN.eval "x" context
        assertEqual ("z", context) $ evalBigInternal EA.eval "x" context
    let context = makeContext [("id", "\\x.x")] in do
        assertEqual ("\\y.y", context) $
                    evalBigInternal EN.eval "(id \\y.y)" context
        assertEqual ("\\y.y", context) $
                    evalBigInternal EA.eval "(id \\y.y)" context
    assertEqual ("\\y#.((\\x.x \\x.y) y#)", empty) $
                evalBigInternal EN.eval "(\\x.\\y.(x y) (\\x.x \\x.y))" empty
    assertEqual ("\\y#.(\\x.y y#)", empty) $
                evalBigInternal EA.eval "(\\x.\\y.(x y) (\\x.x \\x.y))" empty
  where
    evalBigInternal evalSmall expr context
        = first show $ evalBig evalSmall (internal expr) context
    empty = makeContext []

test_evalList :: IO ()
test_evalList =
    let initialContext = makeContext []
        finalContext   = makeContext [("idx", "\\x.x"), ("idy", "\\y.y")]
    in assertEqual (["\\x.x", "\\y.y", "\\y.y"], finalContext) $
                   evalListInternal EN.eval
                                    "idx=\\x.x idy=\\y.y (idx idy)"
                                    initialContext 
  where
    evalListInternal evalSmall exprs context
        = first (map show) $ evalList evalSmall
                                      (fromJust $ parseProgram exprs)
                                      context
