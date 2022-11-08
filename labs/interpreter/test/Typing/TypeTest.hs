{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Typing.TypeTest where

import Util
import Test.Framework

test_similar :: IO ()
test_similar = do
    assertBool $ "a" @~=@ "a" 
    assertBool $ "a" @~=@ "b"
    assertBool $ "(a->a)" @~=@ "(a->a)"
    assertBool $ "(a->a)" @~=@ "(b->b)"
    assertBool $ not $ "(a->a)" @~=@ "(b->c)"
    assertBool $ not $ "a" @~=@ "(a->a)"