{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Typing.InferenceTest where

import Typing.Inference
import Typing.Type
import Typing.Unification
import Util
import qualified Data.Set as S
import Data.Either (isLeft)
import Test.Framework

vars :: Type -> S.Set String
vars (TypeVar x)   = S.singleton x
vars (Arrow t1 t2) = vars t1 `S.union` vars t2

test_copy :: IO ()
test_copy = do
    assertBool $ check "a"
    assertBool $ check "(a->a)"
    assertBool $ check "(a->b)"
    assertBool $ check "(a->(a->a))"
    assertBool $ check "(a->(b->c))"
  where
    check t = t1 ~= t2 && S.null (vars t1 `S.intersection` vars t2)
      where
        t1 = read t
        t2 = copy t1

test_infer :: IO ()
test_infer = do
    -- Missing variable: new type variable is generated
    assertBool $ "a" @~= inferInternal "x" emptyContext emptyContext
    -- Variable found in the local context: the exact type is return
    assertEqual (TypeVar "a") $ inferInternal "x" contextX emptyContext
    -- Variable is found in the global context: a type copy is generated
    let t = inferInternal "x" emptyContext contextX in do
        assertBool $ "a" @~= t
        assertNotEqual (TypeVar "a") t
    -- Function
    assertBool $ "(a->b)" @~= inferInternal "\\x.y" emptyContext emptyContext
    assertBool $ "(a->a)" @~= inferInternal "\\x.x" emptyContext emptyContext
    assertBool $ "(a->(b->a))"
        @~= inferInternal "\\x.\\y.x" emptyContext emptyContext
    assertBool $ "(a->(b->b))"
        @~= inferInternal "\\x.\\y.y" emptyContext emptyContext
    assertBool $ "(a->(b->c))"
        @~= inferInternal "\\x.\\x.y" emptyContext emptyContext
    -- Application
    assertBool $ "a" @~= inferInternal "(x y)" emptyContext emptyContext
    -- Erroneous application in function body
    assertBool $ inferInternalError "\\x.(x x)" emptyContext emptyContext
    -- Definition
    assertBool $ "(a->b)"
        @~= inferInternal "f=\\x.(f x)" emptyContext emptyContext
    -- Erroneous definition
    assertBool $ inferInternalError "f=(f x)" emptyContext emptyContext
    -- Weakly polymorphic local variable
    assertBool $ inferInternalError "\\g.\\x.\\y.((g x) (g y))"
                                    emptyContext emptyContext
    -- Strongly polymorphic global variable
    let global = makeTContext [("id", "(a->a)")] in
        assertBool $ "((a->b)->(a->b))"
            @~= inferInternal "\\x.\\y.((id x) (id y))" emptyContext global
  where
    inferInternal expr local global = typ
      where
        Right typ = infer (internal expr) local global emptySubst 0
    inferInternalError expr local global = isLeft
        $ infer (internal expr) local global emptySubst 0 
    emptyContext = makeTContext []
    contextX     = makeTContext [("x", "a")]
    emptySubst   = makeSubst []
