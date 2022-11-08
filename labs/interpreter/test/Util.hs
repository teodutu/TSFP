module Util where

import Syntax.Expression
import Syntax.Grammar (parseProgram)
import Typing.Type
import qualified Data.Map as M
import Data.Maybe (isJust)

internal :: String -> Expression
internal = maybe (error "Syntax error!") head . parseProgram

makeContext :: [(String, String)] -> Context
makeContext = M.fromList . map (fmap internal)

makeTContext :: [(String, String)] -> TypingContext
makeTContext = M.fromList . map (fmap read)

makeSubst :: [(String, String)] -> Substitution
makeSubst = M.fromList . map (fmap read)

{-|
    Checks whether two types designate the same essential type,
    regardless of the particular type variables employed.
    For instance, (a->(b->a)) and (t0->(t1->t0)) are considered the same,
    i.e. (a->(b->a)) ~= (t0->(t1->t0)).
-}
(~=) :: Type -> Type -> Bool
t1 ~= t2 = isJust $ check M.empty t1 t2
  where
    check :: M.Map String String
          -> Type
          -> Type
          -> Maybe (M.Map String String)
    check assoc (TypeVar x) (TypeVar y)
        | ax == y   = Just assoc
        | ax == x   = Just $ M.insert x y assoc
        | otherwise = Nothing
      where
        ax = M.findWithDefault x x assoc
    check assoc (Arrow u1 u2) (Arrow v1 v2) = do
        assoc' <- check assoc u1 v1
        check assoc' u2 v2
    check _ _ _ = Nothing

{-|
    String-processing variants of (~=).
-}
(@~=) :: String -> Type -> Bool
t1 @~= t2 = read t1 ~= t2

(@~=@) :: String -> String -> Bool
t1 @~=@ t2 = read t1 ~= read t2