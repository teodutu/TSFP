module Typing.Inference where

import Syntax.Expression
import Typing.Type
import Typing.Unification
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Bifunctor (Bifunctor(second))

{-|
    The type of inference state.
    Should comprise:
    * The global typing context
    * The type variable counter.
-}
data TypingState = TypingState
    { context :: TypingContext
    , counter :: Counter
    } deriving Show

{-|
    The type of the inference mechanism.
    Should expose the following:
    * Access to the inference state (State)
    * Acces to the local typing context (Reader)
    * A means for storing unification constraints (Writer)
-}
type Infer = ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState))

runInfer :: Infer a        -- ^ Expression to type
         -> TypingContext  -- ^ Local context
         -> TypingContext  -- ^ Global context
         -> Counter        -- ^ Current type variable counter
         -> (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt = evalState (runWriterT $ runReaderT inf loc) $
                                      TypingState glob cnt

{-|
    Generates a copy of the given type.
    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy t = fst $ runInfer (copyM t) M.empty M.empty 0

{-|
    The type inference function, wich synthesizes the type of the given
    expression.
    Should rely on 'inferM' below.
-}
infer :: Expression          -- ^ Expression to type
      -> TypingContext       -- ^ Local context
      -> TypingContext       -- ^ Global context
      -> Substitution        -- ^ Substitution
      -> Counter             -- ^ Current type variable counter
      -> Either String Type  -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error message
infer expr loc glob subst cnt = let
    inferredTypeM = inferM expr
    (typeExpr, unifications) = runInfer inferredTypeM loc glob cnt
    finalType = foldl unify' (return typeExpr) unifications
    in
        fmap fst $ runUnif finalType subst
    where
        unify' unificationMonadicObject (initType, resultType) = do
            unify initType resultType
            _type <- unificationMonadicObject
            applySubst _type

{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = do
    cnt <- gets counter
    modify $ \t -> t {counter = cnt + 1}
    return $ TypeVar $ "t" ++ show cnt

{-|
    See 'copy'.
-}
copyM :: Type -> Infer Type
copyM (TypeVar x) = do
    ctx <- ask
    case M.lookup x ctx of
        Nothing -> newTypeVar
        Just t  -> return t

copyM (Arrow t1@(TypeVar x) t2) = do
    t1' <- copyM t1
    t2' <- local (M.insert x t1') $ copyM t2
    return $ Arrow t1' t2'

copyM (Arrow t1 t2@(TypeVar x)) = do
    t2' <- copyM t2
    t1' <- local (M.insert x t2') $ copyM t1
    return $ Arrow t1' t2'

copyM (Arrow t1 t2) = Arrow <$> copyM t1 <*> copyM t2

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type
inferM (Var x) = do
    ctx <- ask
    case M.lookup x ctx of
        Just t  -> return t
        Nothing -> do
            globalCtx <- gets context
            case M.lookup x globalCtx of
                Just t  -> copyM t
                Nothing -> newTypeVar

inferM (Lambda x e) = do
    t1 <- newTypeVar
    t2 <- local (M.insert x t1) $ inferM e
    return $ Arrow t1 t2

inferM (Definition x e) = do
    ctx <- gets context
    t <- newTypeVar
    t' <- local (M.insert x t) $ inferM e
    tell [(t, t')]
    modify $ \t1 -> t1 {context = M.insert x t ctx}
    return t'

inferM (Application e1 e2) = do
    t1 <- inferM e1
    t2 <- inferM e2
    t3 <- newTypeVar
    tell [(t1, Arrow t2 t3)]
    return t3
