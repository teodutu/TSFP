module Syntax.Expression where

data Expression
    = Var String
    | Lambda String Expression
    | Application Expression Expression
    | Definition String Expression
    deriving (Read, Show)
