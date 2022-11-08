{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Typing.UnificationTest where

import Typing.Unification
import Util
import Test.Framework

test_chainEnd :: IO ()
test_chainEnd = do
    let subst = makeSubst [] in
        assertEqual "a" $ chainEndInternal subst "a"
    let subst = makeSubst [("a", "b")] in
        assertEqual "b" $ chainEndInternal subst "a"
    let subst = makeSubst [("a", "b"), ("b", "c")] in
        assertEqual "c" $ chainEndInternal subst "a"
    let subst = makeSubst [("a", "b"), ("b", "c"), ("c", "(d->e)")] in
        assertEqual "(d->e)" $ chainEndInternal subst "a"
    let subst = makeSubst [("a", "b"), ("b", "c"), ("c", "(d->e)"), ("d", "f")] in
        assertEqual "(d->e)" $ chainEndInternal subst "a"
  where
    chainEndInternal subst typeVar =
        case runUnif (chainEnd $ read typeVar) subst of
            Right (result, _) -> show result
            Left _            -> error "Left instead of Right"

test_occCheck :: IO ()
test_occCheck = do
    assertEqual False $ occCheckInternal emptySubst "a" "a"
    assertEqual True  $ occCheckInternal emptySubst "a" "b"
    assertEqual False $ occCheckInternal emptySubst "a" "(a->b)"
    assertEqual False $ occCheckInternal emptySubst "a" "(b->a)"
    assertEqual True  $ occCheckInternal bigSubst   "z" "a"
    assertEqual False $ occCheckInternal bigSubst   "d" "a"
    assertEqual False $ occCheckInternal bigSubst   "e" "a"
  where
    occCheckInternal subst typeVar typ =
        case runUnif (occCheck typeVar $ read typ) subst of
            Right (result, _) -> result
            Left _            -> error "Left instead of Right"
    emptySubst = makeSubst []
    bigSubst   = makeSubst [("a", "b"), ("b", "c"), ("c", "(d->e)")]

test_unify :: IO ()
test_unify = do
    assertEqual (Just emptySubst) $ unifyInternal emptySubst "a" "a"
    let resultSubst = makeSubst [("a", "b")] in
        assertEqual (Just resultSubst) $ unifyInternal emptySubst "a" "b"
    let initialSubst = makeSubst [("a", "c")]
        resultSubst  = makeSubst [("a", "c"), ("c", "b")] in
        assertEqual (Just resultSubst) $ unifyInternal initialSubst "a" "b"
    let resultSubst = makeSubst [("a", "(b->c)")] in
        assertEqual (Just resultSubst) $ unifyInternal emptySubst "a" "(b->c)"
    assertEqual Nothing $ unifyInternal emptySubst "a" "(a->b)"
    assertEqual Nothing $ unifyInternal emptySubst "a" "(b->a)"
    let initialSubst = makeSubst [("b", "a")] in
        assertEqual Nothing $ unifyInternal initialSubst "a" "(b->c)"
    let resultSubst = makeSubst [("a", "(b->c)")] in
        assertEqual (Just resultSubst) $ unifyInternal emptySubst "(b->c)" "a"
    assertEqual Nothing $ unifyInternal emptySubst "(a->b)" "a"
    assertEqual Nothing $ unifyInternal emptySubst "(b->a)" "a"
    let initialSubst = makeSubst [("b", "a")] in
        assertEqual Nothing $ unifyInternal initialSubst "(b->c)" "a"
    let resultSubst = makeSubst [("a", "c"), ("b", "d")] in
        assertEqual (Just resultSubst) $ unifyInternal emptySubst "(a->b)" "(c->d)"
    let resultSubst = makeSubst [("a", "b")] in
        assertEqual (Just resultSubst) $ unifyInternal emptySubst "(a->b)" "(b->a)"
  where
    unifyInternal subst t1 t2 =
        case runUnif (unify (read t1) (read t2)) subst of
            Right (_, subst) -> Just subst
            Left _           -> Nothing
    emptySubst = makeSubst []

test_applySubst :: IO ()
test_applySubst = do
    assertEqual "a" $ applySubstInternal emptySubst "a"
    let subst = makeSubst [("a", "b")] in
        assertEqual "b" $ applySubstInternal subst "a"
    let subst = makeSubst [("a", "b")] in
        assertEqual "(b->b)" $ applySubstInternal subst "(a->a)"
    let subst = makeSubst [("a", "b"), ("b", "c"), ("c", "(d->e)"), ("d", "(e->f)")] in
        assertEqual "((e->f)->e)" $ applySubstInternal subst "a"
  where
    applySubstInternal subst typ =
        case runUnif (applySubst $ read typ) subst of
            Right (result, _) -> show result
            Left _            -> error "Left instead of Right"
    emptySubst                   = makeSubst []
