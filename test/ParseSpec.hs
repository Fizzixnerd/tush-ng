{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Test.Hspec

import Unbound.Generics.LocallyNameless

import Language.Tush.Types
import Language.Tush.Parse

parsed :: a -> Result a
parsed = Right

spec :: Spec
spec = parallel $ do
  describe "litP" $ do
    it "parses an Int" $ do
      (parseTush litP "3") `shouldBe` (parsed $ Lit $ LInt 3)
    it "parses a negative Int" $ do
      parseTush litP "-43" `shouldBe` (parsed $ Lit $ LInt $ -43)
    it "parses a Float" $ do
      parseTush litP "34.5" `shouldBe` (parsed $ Lit $ LFloat 34.5)
    it "parses a negative Float" $ do
      parseTush litP "-34.5" `shouldBe` (parsed $ Lit $ LFloat $ -34.5)
    it "parses the root Path" $ do
      parseTush litP "/" `shouldBe` (parsed $ Lit $ LPath $ Path ([], PAbs, FTDirectory))
    it "parses an absolute Path" $ do
      parseTush litP "/hello/world" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PAbs, FTRegular))
    it "parses an absolute directory Path" $ do
      parseTush litP "/hello/world/" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PAbs, FTDirectory))
    it "parses the root relative Path" $ do
      parseTush litP "./" `shouldBe` (parsed $ Lit $ LPath $ Path ([], PRel, FTDirectory))
    it "parses a relative Path" $ do
      parseTush litP "./hello/world" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PRel, FTRegular))
    it "parses a relative directory Path" $ do
      parseTush litP "./hello/world/" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PRel, FTDirectory))
    it "parses the root executable Path" $ do
      parseTush litP "!/" `shouldBe` (parsed $ Lit $ LPath $ Path ([], PExec, FTDirectory))
    it "parses an executable Path" $ do
      parseTush litP "!/hello/world" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PExec, FTRegular))
    it "parses an executable directory Path" $ do
      parseTush litP "!/hello/world/" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PExec, FTDirectory))
    it "parses the root home Path" $ do
      parseTush litP "~/" `shouldBe` (parsed $ Lit $ LPath $ Path ([], PHome, FTDirectory))
    it "parses a home Path" $ do
      parseTush litP "~/hello/world" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PHome, FTRegular))
    it "parses a home directory Path" $ do
      parseTush litP "~/hello/world/" `shouldBe` (parsed $ Lit $ LPath $ Path (["hello", "world"], PHome, FTDirectory))
    it "parses a Char" $ do
      parseTush litP "'c'" `shouldBe` (parsed $ Lit $ LChar 'c')
    it "parses a newline Char" $ do
      parseTush litP "'\\n'" `shouldBe` (parsed $ Lit $ LChar '\n')
    it "parses a single-quote Char" $ do
      parseTush litP "'\''" `shouldBe` (parsed $ Lit $ LChar '\'')
    it "parses True" $ do
      parseTush litP "True" `shouldBe` (parsed $ Lit $ LBool True)
    it "parses False" $ do
      parseTush litP "False" `shouldBe` (parsed $ Lit $ LBool False)
  describe "varP" $ do
    it "parses a normal identifier" $ do
      parseTush varP "hello" `shouldBe` (parsed $ Var $ V (string2Name "hello") Prefix)
    it "parses a symbolic identifier" $ do
      parseTush varP ">>=" `shouldBe` (parsed $ Var $ V (string2Name ">>=") Infix)
    it "parses a backticked identifier" $ do
      parseTush varP "`hello`" `shouldBe` (parsed $ Var $ V (string2Name "hello") InfixBackticks)
  describe "appP" $ do
    it "parses a simple function application" $ do
      parseTush appP "f x" `shouldBe` (parsed $ App (Var $ V (string2Name "f") Prefix) (Var $ V (string2Name "x") Prefix))
    it "parses a multiple application" $ do
      parseTush appP "f x y" `shouldBe` (parsed $ App (App (Var $ V (string2Name "f") Prefix) (Var $ V (string2Name "x") Prefix)) (Var $ V (string2Name "y") Prefix))
    it "parses a symbolic infix application" $ do
      parseTush appP "x + y" `shouldBe` (parsed $ App (App (Var $ V (string2Name "+") Infix) (Var $ V (string2Name "x") Prefix)) (Var $ V (string2Name "y") Prefix))
    it "parses a backticks infix application" $ do
      parseTush appP "x `f` y" `shouldBe` (parsed $ App (App (Var $ V (string2Name "f") InfixBackticks) (Var $ V (string2Name "x") Prefix)) (Var $ V (string2Name "y") Prefix))
    it "parses a mixed application" $ do
      parseTush appP "f x + g y" `shouldBe` (parsed $ App (App (Var (V (string2Name "+") Infix)) (App (Var (V (string2Name "f") Prefix)) (Var (V (string2Name "x") Prefix)))) (App (Var (V (string2Name "g") Prefix)) (Var (V (string2Name "y") Prefix))))
  describe "lamP" $ do
    it "parses the identity function" $ do
      parseTush lamP "\\x -> x" `shouldBe` (parsed $ Lam (bind (PName $ string2Name "x") (Var $ V (string2Name "x") Prefix)))
    it "parses nested lambdas" $ do
      parseTush lamP "\\x -> \\y -> x y" `shouldBe` (parsed $ Lam (bind (PName $ string2Name "x") (Lam (bind (PName $ string2Name "y") (App (Var $ V (string2Name "x") Prefix) (Var $ V (string2Name "y") Prefix))))))
  describe "letP" $ do
    it "parses a simple let" $ do
      parseTush letP "let x = 3 in x" `shouldBe` (parsed $ Let (bind (rec [(PName $ string2Name "x", Embed $ Lit $ LInt 3)]) (Var $ V (string2Name "x") Prefix)))
    it "parses multiple bindings" $ do
      parseTush letP "let x = 4; y = x in y" `shouldBe` (parsed $ Let (bind (rec [(PName $ string2Name "x", Embed $ Lit $ LInt 4), (PName $ string2Name "y", Embed $ Var $ V (string2Name "x") Prefix)]) (Var $ V (string2Name "y") Prefix)))
    it "parses a pattern let" $ do
      parseTush letP "let (A x y) = foo in bar" `shouldBe` (parsed $ Let (bind (rec [((PConstructor (ConstructorName "A") [PName $ s2n "x", PName $ s2n "y"]), Embed $ Var $ V (s2n "foo") Prefix)]) $ Var $ V (s2n "bar") Prefix))
  describe "builtinP" $ do
    it "parses a builtin" $ do
      parseTush builtinP "builtin iadd" `shouldBe` (parsed $ Builtin IAdd)
  describe "typeP" $ do
    it "parses a TCon" $ do
      parseTush typeP "Int" `shouldBe` (parsed $ TCon "Int")
    it "parses a TVar" $ do
      parseTush typeP "a" `shouldBe` (parsed $ TVar (TV "a"))
    it "parse a Symbolic TCon" $ do
      parseTush typeP ">>=" `shouldBe` (parsed $ TCon ">>=")
