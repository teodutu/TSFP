module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars = undefined

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst = undefined