
module Eval where

import Control.Monad
import Control.Applicative

import qualified Data.Map as M

import SExpr
import AParser

------------------------------------------------------------
-- Evaluation

data V = VInt Integer
       | VFun ([V] -> Eval V)

instance Show V where
  show (VInt i) = show i
  show (VFun _) = "<vfun>"

type Env = M.Map String V

newtype Eval a = Eval { runEval :: Env -> Maybe a }

instance Functor Eval where
  fmap f (Eval h) = Eval $ fmap f . h

instance Applicative Eval where
  pure  = return
  (<*>) = ap

instance Monad Eval where
  return a = Eval (const (Just a))
  (Eval f) >>= k  = Eval $ \e ->
    case f e of
      Nothing -> Nothing
      Just x  -> runEval (k x) e

err :: String -> Eval a
err _ = Eval $ const Nothing

getEnv :: Eval Env
getEnv = Eval $ \e -> Just e

liftMaybe :: Maybe a -> Eval a
liftMaybe m = Eval $ const m

local :: (Env -> Env) -> Eval a -> Eval a
local f (Eval e) = Eval (e  . f)

interpretAtom :: Atom -> Eval V
interpretAtom (N n) = return (VInt n)
interpretAtom (I i) = do
  env <- getEnv
  liftMaybe $ M.lookup i env

interpret :: SExpr -> Eval V
interpret (A a)         = interpretAtom a
interpret (Comb [])     = err "Empty combination"
interpret (Comb [A (I "lambda"), args, body]) = do
  as <- getArgs args
  return $ VFun $ \vs ->
    withBindings as vs $ interpret body
interpret (Comb (e:es)) = do
  e' <- interpret e
  case e' of
    VInt _ -> err "Combination head should be a function"
    VFun f -> do
      es' <- mapM interpret es
      f es'

getArgs :: SExpr -> Eval [String]
getArgs (A (I nm)) = return [nm]
getArgs (A _)      = err "Number found where parameter expected"
getArgs (Comb as)  = mapM getIdent as

getIdent :: SExpr -> Eval String
getIdent (A (I nm)) = return nm
getIdent _          = err "Expected a list of parameters"

withBindings :: [String] -> [V] -> Eval a -> Eval a
withBindings names vals = local addBindings
  where addBindings = M.union (M.fromList (zip names vals))

defaultEnv :: Env
defaultEnv = M.fromList
  [ ("plus", VFun (liftI sum))
  ]

liftI :: ([Integer] -> Integer) -> ([V] -> Eval V)
liftI f vs = (VInt . f) <$> liftMaybe (mapM vToInteger vs)
  where
    vToInteger (VInt i) = Just i
    vToInteger _        = Nothing

eval :: SExpr -> Maybe V
eval e = runEval (interpret e) defaultEnv

evalStr :: String -> Maybe V
evalStr s = case runParser parseSExpr s of
              Nothing -> Nothing
              Just (e,_) -> eval e