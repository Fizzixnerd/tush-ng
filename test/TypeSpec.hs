{-# LANGUAGE OverloadedStrings #-}

module TypeSpec where

import Test.Hspec

import Language.Tush.Types
import Language.TushNG
import Language.Tush.Program

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
    it "infers the type of a mutually recusive let" $ do
      checkTush mempty "let odd = \\x -> if builtin ieql (builtin irem x 2) 1 then True else builtin bnot (even x); even = \\x -> if builtin ieql (builtin irem x 2) 0 then True else builtin bnot (odd x) in odd" `shouldBe` (checked $ Forall [] $ TCon "Int" `TArr` TCon "Bool")
    it "infers the type of a user defined data type" $ do
      checkTushProgram "data A = A Int\nmain = A 3" `shouldBe` (checked $ concrete "A")
