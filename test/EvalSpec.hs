{-# LANGUAGE OverloadedStrings #-}

module EvalSpec where

import Test.Hspec

import Language.Tush.Types
import Language.Tush.Eval

spec :: Spec
spec = parallel $ describe "EvalSpec" $ do
  describe "let" $ do
    it "binds names" $ do
      evalTush "let x = 3 in x" `shouldBe` (Right $ Right $ Lit $ LInt 3)
    it "binds multiple names" $ do
      evalTush "let f = \\x -> x; y = 3 in f y" `shouldBe` (Right $ Right $ Lit $ LInt 3)
    it "binds names forward-recursively" $ do
      evalTush "let x = 3; y = x in y" `shouldBe` (Right $ Right $ Lit $ LInt 3)
    it "binds names backward-recursively" $ do
      evalTush "let x = y; y = 3 in x" `shouldBe` (Right $ Right $ Lit $ LInt 3)
    it "binds a recursive function" $ do
      evalTush "let fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1)) in fact 5" `shouldBe` (Right $ Right $ Lit $ LInt 120)
    it "binds mutually recursive functions (1)" $ do
      evalTush "let odd = \\x -> if builtin ieql (builtin irem x 2) 1 then True else builtin bnot (even x); even = \\x -> if builtin ieql (builtin irem x 2) 0 then True else builtin bnot (odd x) in odd 3" `shouldBe` (Right $ Right $ Lit $ LBool True)
    it "binds mutually recursive functions (2)" $ do
      evalTush "let odd = \\x -> if builtin ieql (builtin irem x 2) 1 then True else builtin bnot (even x); even = \\x -> if builtin ieql (builtin irem x 2) 0 then True else builtin bnot (odd x) in odd 2" `shouldBe` (Right $ Right $ Lit $ LBool False)
    it "evals a transformed full Program" $ do
      evalTush "let fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1)); x = 5; main = fact x in main" `shouldBe` (Right $ Right $ Lit $ LInt 120)
  describe "if" $ do
    it "branches correctly (1)" $ do
      evalTush "if True then 1 else 0" `shouldBe` (Right $ Right $ Lit $ LInt 1)
    it "branches correctly (2)" $ do
      evalTush "if False then 1 else 0" `shouldBe` (Right $ Right $ Lit $ LInt 0)
  describe "builtins" $ do
    it "adds properly" $ do
      evalTush "builtin iadd 3 4" `shouldBe` (Right $ Right $ Lit $ LInt 7)


