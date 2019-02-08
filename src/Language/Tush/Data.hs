{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Data where

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Eval
import qualified Language.TushNG as NG

import ClassyPrelude

import Unbound.Generics.LocallyNameless

-- | Takes a Data and returns an Exp transformer that adds the constructors to
-- the lexical environment.
dataToLet :: Data Pattern -> (Exp Pattern -> Exp Pattern)
dataToLet (Data typeName products) = \e -> runFreshM $ do
  productConstructors <- forM products $ \(DataProduct tagName types) -> do
    exps <- foldlM (\(accBdy, names) _ -> do
                       freshName <- fresh $ string2Name "x"
                       return $ (\xs -> Lam (bind (PName freshName) (accBdy xs)), names <> [freshName])) (\xs -> (Lit $ LObject $ Object typeName tagName xs), []) types
    return (exps, tagName)
  let productConstructors' = (\((f, ns), tagName) -> (PName tagName, Embed $ f $ (\n' -> Var $ V n' Prefix) <$> ns)) <$> productConstructors
  return $ Let $ bind (rec productConstructors') e

-- | Takes a Data and returns an Env transformer that adds the types of the
-- constructors to the Env.
dataToEnv :: Data p -> (Env p -> Env p)
dataToEnv (Data typeName products) = \env ->
  let types = (\(DataProduct tagName dataTypes) -> (tagName, NG.generalize env $ foldl' (flip TArr) (TCon $ Name' $ name2String typeName) dataTypes)) <$> products
  in
    NG.extends env types
