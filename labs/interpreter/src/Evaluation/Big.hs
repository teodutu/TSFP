module Evaluation.Big where

import Syntax.Expression
import Data.List ( mapAccumL )
import Data.Tuple ( swap )

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition
evalBig eval exp ctx
    | evalExp == exp = (evalExp, evalCtx)
    | otherwise = evalBig eval evalExp evalCtx
    where
        (evalExp, evalCtx) = eval exp ctx

{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList eval exps ctx = swap $ mapAccumL evalSwap ctx exps
    where
        evalSwap ctx exp = swap $ evalBig eval exp ctx
