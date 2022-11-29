import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Foldable as F
import Data.Map as M

{-
    push :: a -> [a] -> [a]
    push = (:)
    push :: a -> [a] -> ((), [a])
-}
push :: a -> State [a] ()
push a = state $ \s -> ((), a:s)

top :: State [a] a
top = state $ \(x:xs) -> (x, x:xs)

pop :: State [a] a
pop = state $ \(x:xs) -> (x, xs)

{-
    instance Monad (State s) where
        return :: a -> State s a
        return a = State $ \s -> (a, s)

        Creating a stateful operation is like creating a set of "pipes".
        `runState` is the "water" that flows through the pipes.

        (>>=) :: State s a -> (a -> State s b) -> State s b
        State st >>= f = State $ \s ->
            let (a, s') = st s
                in runState (f a) s'

    get :: State s s
    get = State $ \s -> (s, s)

    put :: s -> State s ()
    put s = State $ \_ -> ((), s)

    modify :: (s -> s) -> State s ()
    modify f = State $ \s -> ((), f s)
-}

-- (>>) == (*>)
-- stackOps = push 2 >> push >> 3 >> top >> get >>= push . length

{-
    sequence :: Applicative m => [m a] -> m [a]
    sequence [] = pure []
    sequence (op : ops) = liftA2 (:) op $ sequence ops

    sequence (op : ops) = do
        x <- op
        xs <- sequence ops
        return $ x : xs

    sequence = foldr (liftA2 (:)) (pure [])

    map ::             (a ->   b) -> [a] ->   [b]
    mapM :: Monad m => (a -> m b) -> [a] -> m [b]
        this allows the function to be stateful and have side effects
-}

-- [1, 2, 2, 3, 3, 2, 4, 5, 4] -> we want to remove duplicates
check :: Int -> State [Int] Bool
check x = do
    history <- get
    if x `elem` history
        then return False
        else do
            modify (x:)
            return True

{-
    foldl ::            (a -> b ->   a) -> a -> [b] ->   a
    foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-}

op = runState (foldM (\acc x -> when (x < 0) (modify (x:))
    >> return (acc + x)) 0 [1,2,-3,4,-5]) []

{-
    data Maybe a = Nothing | Just a

    instance Monad Maybe where
        return :: a -> Maybe a
        return = Just

        (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
        Nothing >>= _ = Nothing
        Just a >>= f = f a
-}

{-
    If any valye is`Nothing`, the whole `maybeSeq` is `Nothing`
    maybeSeq = Just 2 >>= \x -> (Nothing >>= \y -> ...)

    Lazy eval comes with implicit exception handling.
-}
maybeSeq = do
    x <- Just 2
    y <- Just 3
    z <- Just 4
    return $ x + y + z

-- Can a sum be paid with coins of 5 and 7? How many of each?
{-
    guard False = empty
    guard True = pure ()

    empty *> op = empty
    op <* empty = empty
-}
payable :: Int -> Maybe (Int, Int)
payable s = lst !! s
    where
        lst = Just (0, 0) : fmap f [1..s]
        f i = fmap (\(f, s) -> (f, s + 1)) (attempt (i - 7)) <|>
              fmap (\(f, s) -> (f + 1, s)) (attempt (i - 5))  -- <|> = alternative
        attempt x = guard (x >= 0) >> lst !! x

{-
    asum :: [f a] -> f a = return the first operation that succeeds
    asum = foldr (<|>) empty
-}

bigFibo :: Monad m => (Int -> m Int) -> (Int -> m Int)
bigFibo _ 0 = return 0
bigFibo _ 1 = return 1
bigFibo f n = liftM2 (+) (f (n - 1)) (f (n - 2))

{-
    newtype Identity a = Identity { runIdentity :: a }
    instance Monad Identity where
        return = Identity
        Identity x >>= f = f x
-}

-- memoises ANY computation, not just fibo
type Memo k v = State (M.Map k v)
bigMemo :: (Ord k) => (k -> Memo k v v) -> (k -> Memo k v v)
bigMemo f k = do
    cache <- get
    case M.lookup k cache of
        Just v -> return v
        Nothing -> do
            v <- f k
            modify (M.insert k v)
            return v

memoFibo n = runState (fix (bigMemo . bigFibo) n) M.empty

{-
    instance Monad [] where
        return x = [x]

        (>>=) :: [a] -> (a -> [b]) -> [b]
        xs >>= f = flip concatMap
-}

-- list comprehension
listOp = do
    x <- [1, 2]
    y <- [3, 4, 5]
    guard $ x + y < 6
    return (x, y)

data DFA q s = DFA
    { initialState :: q
    , transition :: q -> s -> q
    , isFinal :: q -> Bool
    }

data Q = A | B | C | D deriving (Eq, Show)
type Symbol = Char

trans :: Q -> Symbol -> Q
trans A 'b' = B
trans B 'a' = C

dfa :: DFA Q Symbol
dfa = DFA A trans (== C)

runDFA :: DFA q s -> [s] -> Bool
runDFA (DFA i t f) symbols = f $ F.foldl t i symbols

data NFA q s = NFA
    { initialState' :: q
    , transition' :: q -> s -> [q]
    , isFinal' :: q -> Bool
    }

trans' :: Q -> Symbol -> [Q]
trans' A 'b' = [B, D]
trans' B 'a' = [C]

nfa :: NFA Q Symbol
nfa = NFA A trans' (`elem` [C, D])

runNFA :: NFA q s -> [s] -> Bool
runNFA (NFA i t f) symbols = any f $ foldM t i symbols
