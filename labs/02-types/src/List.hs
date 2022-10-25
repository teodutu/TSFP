module List where

import Classes

{-
    Instantiate the following classes with the Haskell list type:
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

instance Container [] where
    contents = id

instance Invertible a => Invertible [a] where
    invert = map invert . reverse
