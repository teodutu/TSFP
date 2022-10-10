{-# OPTIONS_GHC -F -pgmF htfpp #-}
module IntroTest where


import Test.Framework

import {-@ HTF_TESTS @-} BasicsTest
import {-@ HTF_TESTS @-} PicsTest

main :: IO ()
main = htfMain htf_importedTests
