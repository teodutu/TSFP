module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval (Var x) ctx = case M.lookup x ctx of
    Just e -> (e, ctx)
    Nothing -> (Var x, ctx)

eval (Definition f fVal) ctx = (fVal, M.insert f fVal ctx)

eval e@(Lambda _ _) ctx = (e, ctx)

eval (Application (Lambda x body) arg) ctx = (subst x arg body, ctx)
eval (Application e arg) ctx = (Application eEval arg, ctx)
    where
        eEval = fst $ eval e ctx
