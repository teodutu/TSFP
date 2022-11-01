{-|
    A parsing module, based on applicative functors.
-}
module Syntax.Parser where

import Data.Char
import Control.Applicative

{-|
    The type of parsers.

    Each parser is seen as a function taking an input string to a pair between
    the parse result and the rest of the input, if the parsing succeeds, or
    to @Nothing@, if the parsing fails. The function type is wrapped into a
    @newtype@ declaration, for implementation hiding. The parser type is
    parametrized over the type of its result, @a@.
-}
newtype Parser a = P { runParser :: String -> Maybe (a, String) }

-- | Never parses anything, always returning @Nothing@.
failure :: Parser a
failure = P $ const Nothing

{-|
    Parses an explicitly given value, without consuming any input.
    
    Examples:
    
    >>> runParser (success 1) "abc"
    Just (1, "abc")
-}
success :: a -> Parser a
success result = P $ \s -> Just (result, s)

{-|
    Parses a given character.
    
    Examples:
    
    >>> runParser (token 'a') "abc"
    Just ('a', "bc")
    
    >>> runParser (token 'a') "bbc"
    Nothing
-}
token :: Char -> Parser Char
token tok = spot (== tok)
       -- = spot . (==)

{-|
    Parses a character that satisfies a given property.
    
    Examples:
    
    >>> runParser (spot isLetter) "abc"
    Just ('a', "bc")
    
    >>> runParser (spot isLetter) "123"
    Nothing
-}
spot :: (Char -> Bool) -> Parser Char
spot prop = P f
  where
    f [] = Nothing
    f (x : xs)
        | prop x    = Just (x, xs)
        | otherwise = Nothing

instance Functor Parser where
    {-
        Applies a function onto the result of a parser. Also written as (<$>).
        
        It is also said that fmap "lifts" or promotes a function of type
        (a -> b) to the parsing context, yielding a function of type
        (Parser a -> Parser b).

        fmap :: (a -> b) -> Parser a -> Parser b

        Intuitively (but not quite correct):

        fmap :: (a -> b) -> (String -> Maybe (a, String))
                         -> (String -> Maybe (b, String))
    -}
    fmap f (P p) = P $ \s -> fmap (applyToFirst f) $ p s
    {-           = P $ fmap (applyToFirst f) . p
                         ^
                         |
            this fmap corresponds to Maybe

        instance Functor Maybe where
            fmap :: (a -> b) -> Maybe a -> Maybe b
            fmap f (Just x) = Just $ f x
            fmap f Nothing  = Nothing

        Verbose version:

        fmap f (P p) = P $ \s -> case p s of
            Just (res, s') -> Just (f res, s')
            Nothing        -> Nothing
    -}

{-|    
    Parses a letter at the beginning of the input string.

    Examples:
    
    >>> runParser letter "abc"
    Just ('a', "bc")
    
    >>> runParser letter "123"
    Nothing
-}    
letter :: Parser Char
letter = spot isLetter

{-|    
    Parses a digit at the beginning of the input string, and returns
    the result as an @Int@:

    Examples:
    
    >>> runParser digit "123"
    Just (1, "23")
    
    >>> runParser digit "abc"
    Nothing
-}    
digit :: Parser Int
digit = fmap digitToInt $ spot isDigit
  --  = digitToInt <$> spot isDigit

{-
    Applicative functors are stronger than general functors, in their ability
    to sequence several computations.

    For example, if we wanted to sequence two digit parsers (above), a first
    attempt using fmap would yield

    (,) <$> digit :: Parser (a -> (Int, a))

    that is, a Parser wrapping a function which we would have no means
    to extract in order to apply it to the result of the second digit parser.

    Applicatives solve this problem, by providing a way to apply a wrapped
    function onto a wrapped argument, yielding a wrapped result (see (<*>)).

    fmap in Functor can be implemented solely in terms of pure and (<*>),
    meaning that any Applicative is also a Functor:

    fmap f x = pure f <*> x
-}
instance Applicative Parser where
    {-
        The minimum functionality parser that still returns the given value
        as its result. In this case, minimum functionality = no actual parsing.

        pure :: a -> Parser a
    -}
    pure = success
    
    {-
        Performs function application withing a parsing context. Notice that
        the function, of type @(a -> b)@, its argument, of type @a@,
        and its result, of type @b@ are all parsing results.

        Used for sequencing several parsers, and passing their results
        as arguments to another function.

        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -}
    P p <*> P p' = P $ \s -> case p s of
        Just (f, s') -> fmap (applyToFirst f) $ p' s'
        Nothing      -> Nothing
    {-
        Verbose version:

        P p <*> P p' = P $ \s -> case p s of
            Just (f, s') -> case p' s' of
                                Just (r, s'') -> Just (f r, s'')
                                Nothing       -> Nothing
            Nothing      -> Nothing
    -}

    {-
        After implementing the two above, the following two operators come
        for free:

        (*>) :: Parser a -> Parser b -> Parser b
        (<*) :: Parser a -> Parser b -> Parser a

        They sequence two parsers but only keep the results of one. The `>` or
        `<` symbols point toward the parser the result of which is kept.
    -}

{-|
    Parses a letter followed by a digit.

    Examples:

    >>> runParser letterDigit "a12"
    Just (('a', 1), "2")
-}
letterDigit :: Parser (Char, Int)
letterDigit = (,) <$> letter <*> digit
        --  = liftA2 (,) letter digit

{-|
    Parses an expression of the form @\<digit\> \<operator\> \<digit\>@, where
    @\<operator\>@ can be any of +, -, *, /.

    Examples:

    >>> runParser operation "1+2"
    Just ((1, '+', 2), "")
-}
operation :: Parser (Int, Char, Int)
operation = (,,) <$> digit <*> spot (`elem` "+-*/") <*> digit
      --  = liftA3 (,,) digit (spot (`elem` "+-*/")) digit

{-|
    An extended version of 'operation', which also consumes parantheses.

    Examples:

    >>> runParser parOperation "(1+2)"
    Just ((1, '+', 2), "")
-}
parOperation :: Parser (Int, Char, Int)
parOperation = (,,) <$> (token '(' *> digit)
                    <*> spot (`elem` "+-*/")
                    <*> (digit <* token ')')

instance Alternative Parser where
    {-
        The alternative between two parsers. If the first parser succeeds,
        keeps the result. Otherwise, tries the second parser. Notice that
        both parsers need results of the same type.

        (<|>) :: Parser a -> Parser a -> Parser a
    -}
    P p <|> P p' = P $ \s -> maybe (p' s) Just (p s)
    {-
        Verbose version:

        P p <|> P p' = P $ \s -> case p s of
            j@(Just _) -> j
            Nothing    -> p' s
    -}

    {-
        The neutral parser with respect to the alternation operator.
        This is precisely the parser that always fails.

        empty :: Parser a
    -}
    empty = failure

    {-
        After implementing the two above, the following two functions come
        for free:

        many :: Parser a -> Parser [a]
        some :: Parser a -> Parser [a]

        'many' uses a parser to recognize 0 or more occurrences
        of its result. 'some', 1 or more.
    -}

{-|
    Parses an \'A\' in a case insensitive manner.

    Examples:

    >>> runParser insensitiveA "ab"
    Just ('a', "b")

    >>> runParser insensitiveA "Ab"
    Just ('A', "b")
-}
insensitiveA :: Parser Char
insensitiveA = token 'a' <|> token 'A'

{-|
    Parses 1 or more \'A\'s in a case insensitive manner.

    Examples:

    >>> runParser insensitiveAs "aAab"
    Just ("aAa", "b")
-}
insensitiveAs :: Parser String  -- String = [Char]
insensitiveAs = some insensitiveA

{-|
    End of input parser.
-}
eof :: Parser ()
eof = P f
  where
    f "" = Just ((), "")
    f _  = Nothing

{-|
    Applies a parser onto an input string, and returns the result.
    
    Examples:
    
    >>> parse digit "123"
    Just 1
    
    >>> parse digit "abc"
    Nothing
-}
parse :: Parser a -> String -> Maybe a
parse = (fmap fst .) . runParser

{-|
    Applies a function to the first component of a pair.
-}
applyToFirst :: (a -> b) -> (a, c) -> (b, c)
applyToFirst f (x, y) = (f x, y)
