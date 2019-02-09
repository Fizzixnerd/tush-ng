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

step :: Exp FlatPattern -> MaybeT FreshM (Exp FlatPattern)
step (Var _) = done
step (Builtin _) = done
step (App (Lam b) e2) = do
  (x, e1) <- unbind b
  step $ Let (U.bind (rec [(x, Embed e2)]) e1)
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
      -- let (Data f x) = g d; (Data g y) = f d in y
      -- let (Data f x) = g d; (Data g y) = f d in g d
      patternBind :: U.Rec [(FlatPattern, Embed (Exp FlatPattern))]
                  -> FlatPattern
                  -> Exp FlatPattern
                  -> MaybeT FreshM [(Name (Exp FlatPattern), Exp FlatPattern)]
      patternBind _ (FPName name) e = return [(name, e)]
      patternBind _ (FPConstructor patCons names) (Lit (LObject (Object _ valCons vals)))
        = if patCons == valCons
          then return $ zip names vals
          else done
      patternBind bs x y = do
        newY <- step $ Let $ U.bind bs y
        patternBind bs x $ newY
      unembededBindings = (\(x, Embed y) -> (x, y)) <$> bindings
  bindings' <- concat <$> (sequence ((uncurry (patternBind r) <$> unembededBindings)))
  let newBody = substs bindings' body
  -- pExp body >>= traceM . unpack
  -- pExp newBody >>= traceM . unpack
  if newBody == body
    then do
    case runFreshM $ runMaybeT $ step newBody of
      -- both substitutions and reduction didn't change the body, so just return
      -- it.
      Nothing -> return newBody
      Just newBody' -> return $ Let (U.bind (U.rec bindings) newBody')
    else do
    newBody' <- step newBody <|> return newBody
    return $ Let (U.bind (U.rec bindings) newBody')
step (Lit (LObject (Object ty consName es))) = do
  es' <- steps es
  return $ Lit $ LObject $ Object ty consName es'
step (Lit _) = done
step (If (Lit (LBool c)) tru fals)
  = if c
    then return tru
    else return fals
step (If cond tru fals) = If <$> step cond <*> pure tru <*> pure fals

steps :: [Exp FlatPattern] -> MaybeT FreshM [Exp FlatPattern]
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

eval :: Exp FlatPattern -> Exp FlatPattern
eval x = runFreshM (tc step x)

step1 :: Exp FlatPattern -> Exp FlatPattern
step1 x = case runFreshM $ runMaybeT $ step x of
  Nothing -> x
  Just y -> y

evalTush :: Text -> Either (ParseErrorBundle Text Void)
                           (Either (ParseErrorBundle TushTokenStream Void)
                                   (Exp FlatPattern))
evalTush text_ = fmap (eval . runFreshM . flattenPatterns) <$> (parseTush expP text_)

testEvalTush :: Text -> IO ()
testEvalTush text_ = case parseTush expP text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right lexed -> case lexed of
    Left e -> putStr $ pack $ errorBundlePretty e
    Right parsed -> case inferExp mempty $ runFreshM $ flattenPatterns parsed of
      Left e -> do
        print e
        putStrLn $ runFreshM $ pExp pFlatPattern $ eval $ runFreshM $ flattenPatterns $ parsed
      Right scheme -> do
        putStrLn $ prettyPrintScheme scheme
        putStrLn $ runFreshM $ pExp pFlatPattern $ eval $ runFreshM $ flattenPatterns $ parsed
