module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

{-|
    Small-step applicative-order evaluation of a given expression,
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

eval e@(Lambda x body) ctx = (e, ctx)

eval (Application (Lambda x body) arg@(Lambda _ _)) ctx =
    (subst x arg body, ctx)
eval (Application exp@(Lambda _ _) arg) ctx =
    (Application exp evalArg, evalCtx)
        where
            (evalArg, evalCtx) = eval arg ctx
eval (Application e arg) ctx = (Application eEval arg, ctx)
    where
        eEval = fst $ eval e ctx
