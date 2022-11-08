{-# OPTIONS_GHC -F -pgmF htfpp #-}
module InterpreterTest where

import Test.Framework
{-
import {-@ HTF_TESTS @-} Evaluation.SubstitutionTest
import {-@ HTF_TESTS @-} Evaluation.NormalTest
import {-@ HTF_TESTS @-} Evaluation.ApplicativeTest
import {-@ HTF_TESTS @-} Evaluation.BigTest
-}
import {-@ HTF_TESTS @-} Typing.TypeTest
import {-@ HTF_TESTS @-} Typing.UnificationTest
import {-@ HTF_TESTS @-} Typing.InferenceTest

main :: IO ()
main = htfMain htf_importedTests
