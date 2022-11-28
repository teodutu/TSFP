module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

import Control.Monad.State

type Eval = State Context 

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval var@(Var x) ctx = runState (evalM var) ctx

eval def@(Definition f fVal) ctx = runState (evalM def) ctx

eval e@(Lambda _ _) ctx = (e, ctx)

eval (Application (Lambda x body) arg) ctx = (subst x arg body, ctx)
eval (Application e arg) ctx = (Application eEval arg, ctx)
    where
        eEval = fst $ eval e ctx

evalM :: Expression -> Eval Expression

evalM (Var x) = do
    ctx <- get
    return $ M.findWithDefault (Var x) x ctx

evalM (Definition f fVal) = do
    ctx <- get
    return $ put $ M.insert f fVal ctx
