module Typing.Inference where

import Syntax.Expression
import Typing.Type
import Typing.Unification
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

{-|
    The type of inference state.
    
    Should comprise:
    
    * The global typing context
    
    * The type variable counter.
-}
data TypingState = TypingState
    { context :: TypingContext
    , counter :: Counter
    } deriving Show
    
{-|
    The type of the inference mechanism.
    
    Should expose the following:
    
    * Access to the inference state (State)
    
    * Acces to the local typing context (Reader)

    * A means for storing unification constraints (Writer)
-}
type Infer = ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState))

runInfer :: Infer a        -- ^ Expression to type
         -> TypingContext  -- ^ Local context
         -> TypingContext  -- ^ Global context
         -> Counter        -- ^ Current type variable counter
         -> (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt = evalState (runWriterT $ runReaderT inf loc) $ 
                                      TypingState glob cnt

{-|
    Generates a copy of the given type.
    
    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy = undefined

{-|
    The type inference function, wich synthesizes the type of the given
    expression.
    
    Should rely on 'inferM' below.
-}
infer :: Expression          -- ^ Expression to type
      -> TypingContext       -- ^ Local context
      -> TypingContext       -- ^ Global context
      -> Substitution        -- ^ Substitution
      -> Counter             -- ^ Current type variable counter
      -> Either String Type  -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error 
                             --   message.
infer expr loc glob subst cnt = undefined

{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = undefined

{-|
    See 'copy'.
-}
copyM :: Type -> Infer Type
copyM = undefined

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type
inferM = undefined