module Evaluation.NormalError where

import Syntax.Expression
import Syntax.Grammar (parseProgram)
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

type Eval = StateT Context (ExceptT String IO)

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
evalIO :: Expression -> Context -> IO (Expression, Context)
evalIO exp ctx = do
    res <- runExceptT $ runStateT (evalM exp) ctx
    case res of
        Left err -> putStrLn err >> return (exp, ctx)
        Right (exp', ctx') -> return (exp', ctx')

evalM :: Expression -> Eval Expression
evalM (Var x) = do
    ctx <- get
    case M.lookup x ctx of
        Nothing -> throwError $ "Variable " ++ x ++ " not present in context!"
        Just exp -> liftIO (putStrLn "Found variable") >> return exp

evalM (Definition f fVal) = do
    ctx <- get
    put $ M.insert f fVal ctx
    return fVal

evalM (Lambda x body) = return $ Lambda x body

evalM (Application (Lambda x body) arg) = return $ subst x arg body    

evalM (Application f arg) = liftM2 Application (evalM f) (return arg)

internal :: String -> Expression
internal = maybe (error "Syntax error!") head . parseProgram

makeContext :: [(String, String)] -> Context
makeContext = M.fromList . map (fmap internal)

empty = makeContext []
