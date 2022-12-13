module Typing.Unification where

import Typing.Type
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

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
chainEnd var@(TypeVar name) = do
    subst <- get
    case M.lookup name subst of
        Nothing -> return var
        Just t' -> chainEnd t'

chainEnd arrow = return arrow

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck varName t = do
    t' <- chainEnd t
    case t' of
        TypeVar name -> return $ name /= varName
        Arrow t1 t2  -> liftA2 (&&) (occCheck varName t1) (occCheck varName t2)

{-|
    Unifies two type expressions.
-}
unify :: Type     -- ^ First type
      -> Type     -- ^ Second type
      -> Unif ()  -- ^ () if the types unify or an exception otherwise
unify t1 t2 = do
    t1' <- chainEnd t1
    t2' <- chainEnd t2
    case (t1', t2') of
        (TypeVar name1, TypeVar name2) -> do
            subst <- get
            if name1 == name2
                then return ()
                else put $ M.insert name1 t2' subst
        (TypeVar name, t) -> do
            subst <- get
            if M.member name subst
                then throwError $ "Type variable " ++ name ++ " is already bound"
                else do
                    occ <- occCheck name t
                    if occ
                        then put $ M.insert name t subst
                        else throwError $ "Type variable " ++ name ++ " occurs in " ++ show t
        (t, TypeVar name) -> unify t2 t1
        (Arrow t1 t2, Arrow t3 t4) -> do
            unify t1 t3
            unify t2 t4

{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type       -- ^ Target type
           -> Unif Type  -- ^ Resulting type
applySubst t = do
    t' <- chainEnd t
    case t' of
        TypeVar name -> do
            subst <- get
            case M.lookup name subst of
                Nothing -> return t'
                Just t'' -> applySubst t''
        Arrow t1 t2 -> liftA2 Arrow (applySubst t1) (applySubst t2)
