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

import ClassyPrelude

import Unbound.Generics.LocallyNameless as U

import Control.Monad.Trans.Maybe
import Text.Megaparsec

lam :: String -> Exp -> Exp
lam x t = Lam $ U.bind (string2Name x) t

var :: String -> Exp
var x = Var $ V (string2Name x) Prefix

done :: MonadPlus m => m a
done = mzero

step :: Exp -> MaybeT FreshM Exp
step (Var _) = done
step (Builtin _) = done
step (App (Lam b) e2) = do
  (x, e1) <- unbind b
  return $ subst x e2 e1
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
step (App e1 e2) = App <$> step e1 <*> pure e2
                   <|> App <$> pure e1 <*> step e2
step (Lam _) = done
step (Let binds) = do
  (r, body) <- unbind binds
  let bindings = unrec r
      newBody = substs ((\(x, Embed y) -> (x, y)) <$> bindings) body
  -- pExp body >>= traceM . unpack
  -- pExp newBody >>= traceM . unpack
  if newBody == body
    then return newBody
    else do
    newBody' <- step newBody
    return $ Let $ U.bind (U.rec bindings) newBody'
step (Lit _) = done
step (If (Lit (LBool c)) tru fals)
  = if c
    then return tru
    else return fals
step (If cond tru fals) = If <$> step cond <*> pure tru <*> pure fals
step (Fix (Lam b)) = do
  (x, e) <- unbind b
  return $ subst x (Fix (Lam b)) e
step (Fix x) = Fix <$> step x

-- | Transitive closure
tc :: (Monad m, Functor m) => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
  ma' <- runMaybeT $ f a
  case ma' of
    Just a' -> tc f a'
    Nothing -> return a

eval :: Exp -> Exp
eval x = runFreshM (tc step x)

testEvalTush :: Text -> IO ()
testEvalTush text_ = case parseTush expP text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right lexed -> case lexed of
    Left e' -> putStr $ pack $ errorBundlePretty e'
    Right parsed -> case inferExp mempty parsed of
      Left e'' -> do
        print e''
        putStrLn $ runFreshM $ pExp $ eval parsed
      Right scheme -> do
        putStrLn $ prettyPrintScheme scheme
        putStrLn $ runFreshM $ pExp $ eval parsed
