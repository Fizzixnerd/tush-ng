{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Tush.Eval where

import Language.Tush.Types
import Language.Tush.Parse
import Language.TushNG
import Language.Tush.Pretty
import Language.Tush.Reduce
import Language.Tush.Result

import ClassyPrelude

import Unbound.Generics.LocallyNameless as U

import Control.Monad.Trans.Maybe
import Data.Void
import Text.Megaparsec

lam :: String -> Exp Pattern -> Exp Pattern
lam x t = Lam $ U.bind (PName $ string2Name x) t

var :: String -> Exp p
var x = Var $ V (string2Name x) Prefix

done :: MonadPlus m => m a
done = mzero

step :: Exp PlainName -> MaybeT FreshM (Exp PlainName)
step (Var _) = done
step (Builtin _) = done
step (App (Lam b) e2) = do
  (x, e1) <- unbind b
  return $ Let (U.bind (rec [(x, Embed e2)]) e1)
step (App (App (Builtin b) (Lit (LInt x))) (Lit (LInt y))) = do
  case b of
    IAdd -> return $ Lit $ LInt (x + y)
    ISub -> return $ Lit $ LInt (x - y)
    IMul -> return $ Lit $ LInt (x * y)
    IDiv -> return $ Lit $ LInt (x `div` y)
    IRem -> return $ Lit $ LInt (x `rem` y)
    IEql -> return $ Lit $ LBool (x == y)
    INeq -> return $ Lit $ LBool (x /= y)
    _ -> done
step (App (Builtin BNot) (Lit (LBool b))) = return $ Lit $ LBool $ not b
step (App (App (Builtin BXor) (Lit (LBool x))) (Lit (LBool y))) = return $ Lit $ LBool $ x /= y
step (App (App (Builtin ONth) (Lit (LInt n))) (Lit (LObject (Object _ _ xs)))) = return $ xs `unsafeIndex` (fromIntegral n)
step (App e1 e2) = App <$> step e1 <*> pure e2
                   <|> App <$> pure e1 <*> step e2
step (Lam _) = done
step (Let binds) = do
  (r, body) <- unbind binds
  let bindings = unrec r
      unembededBindings = (\(PlainName x, Embed y) -> (x, Let $ U.bind r y)) <$> bindings
      newBody = substs unembededBindings body
  return newBody
step (Lit (LObject (Object ty consName es))) = do
  es' <- steps es
  return $ Lit $ LObject $ Object ty consName es'
step (Lit _) = done
step (If (Lit (LBool c)) tru fals)
  = if c
    then return tru
    else return fals
step (If cond tru fals) = If <$> step cond <*> pure tru <*> pure fals

steps :: [Exp PlainName] -> MaybeT FreshM [Exp PlainName]
steps (x:xs) = do
  let steppedX = runFreshM $ runMaybeT $ step x
  case steppedX of
    Just x' -> return (x' : xs)
    Nothing -> do
      xs' <- steps xs
      return $ x : xs'
steps [] = mzero

-- | Transitive closure
tc :: (Monad m, Functor m) => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
  ma' <- runMaybeT $ f a
  case ma' of
    Just a' -> tc f a'
    Nothing -> return a

showSteps :: Exp PlainName -> IO ()
showSteps a = do
  putStrLn $ runFreshM $ pExp pPlainName a
  let ma' = runFreshM $ runMaybeT $ step a
  case ma' of
    Just a' -> do
      showSteps a'
    Nothing -> do
      return ()

showStepsRaw :: Exp PlainName -> IO ()
showStepsRaw a = do
  putStrLn $ tshow a
  let ma' = runFreshM $ runMaybeT $ step a
  case ma' of
    Just a' -> do
      showStepsRaw a'
    Nothing -> do
      return ()


eval :: Exp PlainName -> Exp PlainName
eval x = runFreshM (tc step x)

step1 :: Exp PlainName -> Exp PlainName
step1 x = case runFreshM $ runMaybeT $ step x of
  Nothing -> x
  Just y -> y

evalTush :: Text -> Result (Exp PlainName)
evalTush text_ = (eval . runFreshM . (removePatterns <=< flattenPatterns)) <$> (parseTush expP text_)

testEvalTush :: Text -> IO ()
testEvalTush text_ = putStrLn $ prettyResult (runFreshM . pExp pPlainName) $ evalTush text_
