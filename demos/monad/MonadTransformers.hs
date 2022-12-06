{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

import Control.Applicative
import Control.Monad.Identity
import Data.Foldable as F
import Data.Map as M

newtype State s a  = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
    return a = State $ \s -> (a, s)

    State st >>= f = State $ \s ->
        let (a, s') = st s
            in runState (f a) s'

{-
    getLine :: IO String
-}

ioSeq = do
    line <- getLine
    putStrLn $ "string is: " ++ line

{-
    We can define get and put in termns of `modify` or `modify` in terms of
    `get` and `put`.
-}
class Monad m => MonadState s m | m -> s where
    get :: m s -- instance MonadState (State s)
    get = state $ \s -> (s, s)

    put :: s -> m ()
    put s = state $ \_ -> ((), s)

    -- modify :: (s -> s) -> m ()
    -- modify f = State $ \s -> ((), f s)

    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get
        let (a, s') = f s
        put s'
        return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = do
    s <- get
    put $ f s

instance MonadState s (State s) where
    state = State

-- state transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

{-
    1. m (s -> (a, s))
    2. s -> m (a, s)
    3. s -> (m a, s)
-}

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    -- uses the return from the monad m
    return a = StateT $ \s -> return (a, s)

    -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    StateT st >>= f = StateT $ \s -> do
        (a, s') <- st s
        runStateT (f a) s'

instance Monad m => MonadState s (StateT s m) where

    -- state f = StateT $ \s -> return $ f s
    state f = StateT $ return . f

type Parser = StateT String Maybe

-- runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

token :: Char -> Parser Char
token c = do
    s <- get
    case s of
        (x:xs)
            | x == c -> put xs >> return c
        {-
            We need to employ operations from the underlying monad and use them
            in the compound monad.
        -}
        _ -> lift Nothing

class MonadTrans t where
    -- Promotes an operation from the inner monad to the wrapped ensemble.
    lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
    -- lift :: Monad m => m a -> StateT s m a
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

{-
    newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

    Parser a  = MaybeT (State String) a
    State String (Maybe a)
    String -> Maybe (a, String)

    We don't have an implicit mechanism to automatically stop the computation
    when we encounter an error. So the order matters.
-}

{-
    data Either e a = Left e | Right a  -- disjoint union

    Either Int Int = { Left 0, Right 0, Left 1, Right 1, ... }
-}
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "Division by zero"
safeDiv x y = Right $ x `div` y

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance Monad m => Functor (ExceptT e m) where
    fmap = liftM

instance Monad m => Applicative (ExceptT e m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
    -- return :: a -> ExceptT e m a
    return = ExceptT . return . Right

    -- (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
    ExceptT m >>= f = ExceptT $ do
        eea <- m
        case eea of
            Right a -> runExceptT $ f a
            Left e -> return $ Left e

instance Monad m => MonadError e (ExceptT e m) where
    -- throwError :: e -> ExceptT e m a
    throwError = ExceptT . return . Left

    -- catchError :: ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
    catchError (ExceptT m) f = ExceptT $ do
        eea <- m
        case eea of
            r@(Right a) -> return $ r
            Left e -> runExceptT $ f e

instance MonadTrans (ExceptT e) where
    -- lift :: Monad m => m a -> ExceptT e m a
    lift = ExceptT . fmap Right

getLine3 :: ExceptT String IO String
getLine3 = do
    line <- lift getLine
    if length line < 3
        then throwError "Line too short"
        else return line

excIoSeq = do
    line1 <- getLine3
    line2 <- getLine3
    return $ line1 ++ line2

-- We can define data types with detailed information about the error.
