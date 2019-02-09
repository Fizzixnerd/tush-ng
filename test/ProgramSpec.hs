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

evaled = Right . Right

spec :: Spec
spec = parallel $ do
  describe "programToExp" $ do
    it "runs a sample program" $ do
      runFile "tush/test.tush" `shouldReturn` (evaled $ Lit $ LInt 120)
    it "runs a program with recursive ADTs" $ do
      runFile "tush/recursive_adts.tush" `shouldReturn` (evaled $ Lit $ LObject $ Object (s2n "Tree") (s2n "Node") [Lit $ LObject $ Object (s2n "Tree") (s2n "Leaf") [Lit $ LInt 4], Lit $ LObject $ Object (s2n "Tree") (s2n "Leaf") [Lit $ LInt 3]])
