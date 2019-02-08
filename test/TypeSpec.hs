{-# LANGUAGE OverloadedStrings #-}

module TypeSpec where

import Test.Hspec

import Language.Tush.Types
import Language.TushNG

checked = Right . Right . Right

concrete x = Forall [] (TCon x)

spec :: Spec
spec = do
  describe "infer" $ do
    it "infers the type of an Int" $ do
      checkTush mempty "3" `shouldBe` (checked $ concrete "Int")
    it "infers the type of a Float" $ do
      checkTush mempty "1.45" `shouldBe` (checked $ concrete "Float")
    it "infers the type of a String" $ do
      checkTush mempty "\"hello\"" `shouldBe` (checked $ concrete "String")
    it "infers the type of a Char" $ do
      checkTush mempty "'c'" `shouldBe` (checked $ concrete "Char")
    it "infers the type of a Path" $ do
      checkTush mempty "/hello/there" `shouldBe` (checked $ concrete "Path")
    it "infers the type of the identity" $ do
      checkTush mempty "\\x -> x" `shouldBe` (checked $ Forall [TV "a"] $ TVar "a" `TArr` TVar "a")
    it "infers the type of an application of the identity to an Int" $ do
      checkTush mempty "(\\x -> x) 3" `shouldBe` (checked $ concrete "Int")
    it "infers the type of a simple let" $ do
      checkTush mempty "let x = 3 in x" `shouldBe` (checked $ concrete "Int")
