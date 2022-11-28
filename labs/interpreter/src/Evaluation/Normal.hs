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
eval exp ctx = runState (evalM exp) ctx

evalM :: Expression -> Eval Expression
evalM (Var x) = do
    ctx <- get
    return $ M.findWithDefault (Var x) x ctx

evalM (Definition f fVal) = do
    ctx <- get
    put $ M.insert f fVal ctx
    return fVal

evalM (Lambda x body) = do
    ctx <- get
    return $ Lambda x body

evalM (Application (Lambda x body) arg) = do
    ctx <- get
    return $ subst x arg body

evalM (Application f arg) = do
    ctx <- get
    evalF <- evalM f
    return $ Application evalF arg
