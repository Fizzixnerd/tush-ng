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

evaled :: a -> Result a
evaled = return

runFile' :: MonadIO m => FilePath -> m (Result (Exp PlainName))
runFile' x = do
  result <- runFile x
  return $ fst <$> result

spec :: Spec
spec = parallel $ do
  describe "programToExp" $ do
    it "runs a sample program" $ do
      runFile' "tush/test.tush" `shouldReturn` (evaled $ Lit $ LInt 120)
    it "runs a program with recursive ADTs" $ do
      let same = do
            feExpScheme <- runFileM "tush/recursive_adts.tush"
            return $ runFreshM $ do
              eExpScheme <- feExpScheme
              return $ do
                (ex, _) <- eExpScheme
                let expected = Lit $ LObject $ Object "Tree" (ConstructorName "Node") [Lit $ LObject $ Object "Tree" (ConstructorName "Leaf") [Lit $ LInt 3], Lit $ LObject $ Object "Tree" (ConstructorName "Leaf") [Lit $ LInt 4]]
                return $ aeq ex expected
      same `shouldReturn` (evaled $ True)
