module Parser where

import Data.Char
import Control.Applicative

{-
    We need a parser that can also handle errors. We use an `Either` type
    where the left value is an error message and the right value is the useful
    parsing result.

    Its input should be Strings and the return type should be tokens or Strings
    to continue parsing

    -
    `runParser` takes an object of type Parser a and a String and returns a
    parsed object.
-}
newtype Parser a = P { runParser :: String -> Maybe (a, String) }

failure :: Parser a
failure = P { runParser = const Nothing }


-- `success` doesn't consume any input
success :: a -> Parser a
success result = P $ \s -> Just (result, s)

token :: Char -> Parser Char
token = spot . (==)

spot :: (Char -> Bool) -> Parser Char
spot p = P $ \s -> case s of
    t : ts
        | p t -> Just (t, ts)
    _ -> Nothing

{-
    instance Functor Maybe where
        fmap :: (a -> b) -> Maybe a -> Maybe b
        fmap _ = Nothing = Nothing
        fmap f (Just a) = Just $ f a

    `fmap`ping a function to a function container (such as `runParser`) is the
    same as composing the function with the function container.
-}
instance Functor Parser where
    fmap f (P p) = P $ \s -> fmap (\(a, s') -> (f a, s')) $ p s

{-
    <$> is the same as $ but applied to the contents of `spot isDigit`, not onto
    `spot isDigit` itself
-}
digit :: Parser Int
digit = digitToInt <$> spot isDigit

letter :: Parser Char
letter = spot isLetter

{-
    Exposes (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-}
instance Applicative Parser where
    {-
        For the Parser to be a Functor, we need to be able to apply `fmap` with
        the functions defined here. We can't do this with `<*>` alone because
        it only takes "boxed" arguments. We need something with which to wrap
        it: `pure`. `pure` just boxes the argument within the Parser.

        `success` is the simplest parser: it doesn't consume any input.

        pure :: a -> Parser a
    -}
    pure = success

    {-
        (*>) :: Parser a -> Parser b -> Parser b
        (<*) :: Parser a -> Parser b -> Parser a

        The 2 "halves" of (<*>) come for free. They ignore the contents of the
        parser. The > and < operators are used to point to the parser whose
        contents we keep.
    -}
    P p <*> P p' = P $ \s -> case p s of
        Nothing -> Nothing
        Just (f, s') -> fmap (\(a, s'') -> (f a, s'')) $ p' s'

    {-
        We now get `some` and `many` for free.

        some :: Parser a -> Parser [a]
        many :: Parser a -> Parser [a]

        some p = liftA2 (:) p (many p)  <=> (:) <$> p <*> many p
        many p = some p <|> pure []
    -}

{-
    fmap :: Functor f => (a -> b) -> f a -> f b
    (,) :: c -> d -> (c, d)
    letter :: Parser Char
    => fmap (,) letter :: Parser (d -> (Char, d))

    letterDigit = fmap (,) letter <*> digit
    letterDigit = (,) <$> letter <*> digit

    liftA :: Applicative f => (a -> b) -> f a -> f b
    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-}
letterDigit :: Parser (Char, Int)
letterDigit = liftA2 (,) letter digit

-- Recognises parentheses
parOperation :: Parser (Int, Char, Int)
-- parOperation = liftA3 (,,) digit token digit
parOperation = (,,) <$>
    (token '(' *> digit) <*>
    (spot (`elem` "+-*/")) <*>
    (digit <* token ')')

{-
    Exposes the (<|>) operator, which chooses the output of the first parser if
    it succeeds, or the second parser if the first one fails.
-}
instance Alternative Parser where
    {-
        So that we can choose the other parser (with Alternative) if the first
        one fails.
    -}
    empty = failure

    P p <|> P p' = P $ \s -> case p s of
        j@(Just _) -> j
        Nothing -> p' s

insensitiveA :: Parser Char
insensitiveA = token 'a' <|> token 'A'

insensitiveAs :: Parser String
insensitiveAs = some insensitiveA

{-
    () is the "unit type". It's like a `void` type in C. Its only value is `()`.
    It's used to represent the absence of a value.
-}
eof :: Parser ()
eof = P $ \s -> case s of
    [] -> Just ((), "")
    _ -> Nothing

-- Applies a function to the first element of a pair
applyToFirst :: (a -> b) -> (a, c) -> (b, c)
applyToFirst f (a, c) = (f a, c)

{-
    Applies a function only to the first token, while disregarding the rest.
    `runParser` also returns the rest of the string.

    `parse` returns the first element of the tuple returned by `runParser`.
-}
parse :: Parser a -> String -> Maybe a
parse p s = fst <$> runParser p s

data Expr
    = Literal Int
    | Add Expr Expr
    deriving (Read, Show)

{-
    This is like a small DSL defined in Haskell.
-}
literal :: Parser Expr
literal = Literal <$> digit  -- fmap `Literal` onto `digit`

add :: Parser Expr
add = liftA2 Add (token '(' *> expr <* token '+') (expr <* token ')')

expr :: Parser Expr
expr = add <|> literal

{-
    We must pay attention to not accept definitions outside the global scope,
    i.e.:
        - this is fine: id=\x.(x x)
        - this is not: (\x.x \y.y)
-}
data LambdaExpr
    = Var String
    | Lambda String LambdaExpr


{-
    Monads need a few things to be defined:
        - return :: a -> m a
        - (>>=) :: m a -> (a -> m b) -> m b

    `return` is the same as `pure` in Applicative
    (>>=) is also called "bind" in Monad
-}

instance Monad Parser where
    return = pure

    {-
        Parser a -> (a -> Parser b) -> Parser b
            This is "P bound to f"
        fmap f p :: Parser (Parser b)
        join :: Monad m => m (m a) -> m a
            >>= == join . fmap, so we can either define return and (>>=) or
            return and join. Both are complete definitions of monads.
    -}
    P p >>= f = P $ \s -> case p s of
        Nothing -> Nothing
        Just (a, s') -> runParser (f a) s'

{-
    "1 1 + 2"
    "2 1 * 2"
    "1 1 1 + 2"
-}
-- seq1 = digit >>= \n if n == 1 then liftA3 (,,) digit (token '+') digit
--                               else liftA3 (,,) digit (token '*') digit
seq1 = digit >>= \n liftA3 (,,) digit (token (if n == 1 then '+' else '*')) digit

{-
    do
        x <- p1  -- extracts the result of p1
        p2
    is the same as:
    p1 >>= \x -> p2

    do
        x <- p1
        y <- p2
        p3
    is the same as:
    p1 >>= \x -> p2 >>= \y -> p3
    p1 >>= \x -> (p2 >>= \y -> p3)  -- `x` is visible in `p3`

    do
        x <- p1
        p2
        y <- p3
    is the same as:
    p1 >>= \x -> p2 >> p3 >>= \y -> return ()
    (>>) = (*>)

    op1 >> op2 = op1 >>= const op2
-}
seq2 = do  -- syntactic sugar for `>>=`
    n <- digit  -- extracts the value from the Parser
    -- monadic `let`. It is visible for the rest of the `do` block.
    -- This is just an alias
    let x = digit
    if n == 1 then liftA3 (,,) digit (token '+') digit
              else do
                a <- digit
                token '*'
                b <- digit
                return (a, '*', b)

{-
    Every Applicative is a Functor
        we can implement fmap with `pure` and `<*>`
    Every Monad is an Applicative
        pure = return
        pf <*> px = do
            f <- pf
            x <- px
            return $ f x
-}

{-
    f :: a -> b
    g :: b -> c
    g . f :: a -> c

    f :: a -> Parser b
    g :: b -> Parser c
    g "." f :: a -> Parser c
    We want the compisition to mix the results of the two parsers.
    We'll use the "fish operator": (<=<) that is used to compose monads.

    g <=< f = \a -> do
        b <- f a
        g b
    g <=< f = \a -> f a >>= g
-}
