module Syntax.Grammar where

import Control.Applicative
import Data.Char

import Syntax.Expression
import Syntax.Parser

parseProgram :: String -> Maybe [Expression]
parseProgram = parse $
    many ((parseDefinition <|> parseExpression)
            <* ignoreSpaces)
        <* eof

ignoreSpaces :: Parser String
ignoreSpaces = many $ spot isSpace

parseDefinition :: Parser Expression
parseDefinition = liftA2 Definition
    (ignoreSpaces *> parseVar <* ignoreSpaces)
    (token '=' *> ignoreSpaces *> parseExpression <* ignoreSpaces)

parseVar :: Parser String
parseVar = some $ spot $ isLetter

parseExpression :: Parser Expression
parseExpression = parseApplication <|> parseLambda <|> liftA Var parseVar

parseApplication :: Parser Expression
parseApplication = liftA2 Application
    (token '(' *> ignoreSpaces *> parseExpression <* ignoreSpaces)
    (parseExpression <* ignoreSpaces <* token ')')

parseLambda :: Parser Expression
parseLambda = liftA2 Lambda
    (token '\\' *> ignoreSpaces *> parseVar <* ignoreSpaces)
    (token '.' *> ignoreSpaces *> parseExpression <* ignoreSpaces)
