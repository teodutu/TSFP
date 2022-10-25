module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a
    = Empty
    | Value a
    | List [NestedList a]

instance Show a => Show (NestedList a) where
    show Empty = "[]"
    show (Value x) = show x
    show (List xs) = "[" ++ showList xs ++ "]"
        where
            showList [] = ""
            showList [x] = show x
            showList (x:xs) = show x ++ ", " ++ showList xs

instance Container NestedList where
    contents Empty = []
    contents (Value x) = [x]
    contents (List xs) = concatMap contents xs

instance Invertible a => Invertible (NestedList a) where
    invert Empty = Empty
    invert (Value x) = Value (invert x)
    invert (List xs) = List (map invert $ invert xs)

instance Functor NestedList where
    fmap _ Empty = Empty
    fmap f (Value x) = Value (f x)
    fmap f (List xs) = List (map (fmap f) xs)

l = List [Value 1, List [Value 2, Value 3], Value 4, Value 5]
