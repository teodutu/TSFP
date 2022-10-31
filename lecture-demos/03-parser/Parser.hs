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
