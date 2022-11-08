module Typing.Unification where

import Typing.Type
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}
type Unif = StateT Substitution (Except String)

runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:
    
    * an unbound type variable
    
    * a function type.
-}
chainEnd :: Type       -- ^ Type to look up
         -> Unif Type  -- ^ Chain end
chainEnd = undefined

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck = undefined

{-|
    Unifies two type expressions.
-}
unify :: Type     -- ^ First type
      -> Type     -- ^ Second type
      -> Unif ()  -- ^ () if the types unify or an exception otherwise
unify = undefined

{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type       -- ^ Target type
           -> Unif Type  -- ^ Resulting type
applySubst = undefined
