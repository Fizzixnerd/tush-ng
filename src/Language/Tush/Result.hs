{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Result where

import ClassyPrelude

import Text.Megaparsec

import Unbound.Generics.LocallyNameless
import Language.Tush.Types
import Language.Tush.Pretty

prettyTushError :: TushError -> Text
prettyTushError e = case e of
  LexErr err -> pack $ errorBundlePretty err
  ParseErr err -> pack $ errorBundlePretty err
  -- TODO: Prettify TypeErrors
  TypeErr err _ -> runFreshM $ (pTypeError pFlatPattern err :: FreshM Text)

prettyResult :: (a -> Text) -> Result a -> Text
prettyResult _ (Left e) = prettyTushError e
prettyResult pretty (Right x) = pretty x
