{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgramSpec where

import Test.Hspec

import ClassyPrelude

import Unbound.Generics.LocallyNameless

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Eval
import Language.Tush.Program

spec :: Spec
spec = parallel $ describe "ProgramSpec" $ do
  describe "programToExp" $ do
    it "runs a sample program" $ do
      (do
          programText <- readFileUtf8 "tush/test.tush"
          return $ fmap (eval . programToExp) <$> (parseTush programP programText)) `shouldReturn` (Right $ Right $ Lit $ LInt 120)
