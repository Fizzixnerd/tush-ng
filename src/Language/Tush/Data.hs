{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Data where

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Eval
import Language.Tush.Pretty
import qualified Language.TushNG as NG

import ClassyPrelude

import Unbound.Generics.LocallyNameless

dataToDataLet :: Data Pattern -> [(Name (Exp Pattern), Embed (Exp Pattern), Scheme)]
dataToDataLet (Data dataType products) = productToDatEntry dataType <$> products

productToDatEntry :: Type -> DataProduct Pattern -> (Name (Exp Pattern), Embed (Exp Pattern), Scheme)
productToDatEntry t@(TCon (Name' dataType)) (DataProduct name types)
  = let (toBody, varNames) = runFreshM $ foldM (\(accBdy, names) _ -> do
                                                   freshName <- fresh $ string2Name "x"
                                                   -- This reverses the arguments!
                                                   return $ (\xs -> Lam (bind (PName freshName) (accBdy xs)), names <> [freshName])) (\xs -> (Lit $ LObject $ Object dataType (ConstructorName $ name2String name) xs), []) types
    in
      (name, Embed $ toBody $ (\n -> Var $ V n Prefix) <$> reverse varNames, Forall [] $ NG.arrow (types ++ [t]))
productToDatEntry ty _ = terror $ "Cannot name a type '" ++ pType ty ++ "'."
