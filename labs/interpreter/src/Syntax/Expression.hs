module Syntax.Expression where

data Expression
    = Var String
    | Lambda String Expression
    | Application Expression Expression
    | Definition String Expression
    deriving (Read, Eq)

instance Show Expression where
    show (Var x) = x
    show (Lambda x e) = "\\" ++ x ++ "." ++ show e
    show (Application e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Definition x e) = x ++ " = " ++ show e
