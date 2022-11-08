module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars = toList . freeVarsSet

freeVarsSet :: Expression -> Set String
freeVarsSet (Var x) = singleton x
freeVarsSet (Lambda x e) = delete x $ freeVarsSet e
freeVarsSet (Application e1 e2) = union (freeVarsSet e1) (freeVarsSet e2)

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst x exp arg@(Var y) = if x == y then exp else arg
subst x exp (Application e1 e2) = Application (subst x exp e1) (subst x exp e2)
subst x exp arg@(Lambda y body)
    | x == y = arg
    | member y (freeVarsSet exp) =
        Lambda y' (subst x exp (subst y (Var y') body))
    | otherwise = Lambda y (subst x exp body)
    where
        y' = y ++ "#"
