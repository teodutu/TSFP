module Typing.Type where

import Syntax.Parser
import Control.Applicative
import qualified Data.Map as M

data Type
    = TypeVar String   -- Type variable
    | Arrow Type Type  -- Function type
    deriving Eq
    
instance Show Type where
    show (TypeVar v)   = v
    show (Arrow t1 t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"

instance Read Type where
    readsPrec _ = maybe [] (: []) . runParser typ
      where
        typ, typeVar, arrow :: Parser Type
        typ     = typeVar <|> arrow
        typeVar = TypeVar <$> some letter
        arrow   = liftA2 Arrow (token '(' *> typ)
                               (token '-' *> token '>' *> typ <* token ')')
                               
-- | Binds TYPE variables to types
type Substitution = M.Map String Type

-- | Binds PROGRAM variables to types
type TypingContext = M.Map String Type

-- | Counter for generated type variables
type Counter = Int