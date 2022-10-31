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

success :: a -> Parser a
success result = P $ runParser = \s -> Just (result, s)

token :: Char -> Parser Char
token = spot . (==)

spot :: (Char -> Bool) -> Parser Char
spot p = P $ \s -> case s of
    t : ts
        | p t -> Just (t, ts)
    _ -> otherwise -> Nothing

instance Functor Parser where
    fmap f (P p) = P $ \s -> fmap (\(a, s') -> (f a, s')) $ p s
