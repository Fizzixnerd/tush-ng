{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Program where

import Unbound.Generics.LocallyNameless as U

import Text.Megaparsec

import Data.Void
import Unsafe.Coerce

import ClassyPrelude

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Pretty
import Language.Tush.Eval
import Language.Tush.Reduce
import Language.Tush.Data
import Language.TushNG

programToExp :: Program Pattern -> Exp Pattern
programToExp (Program defs)
  = let binds = [(PName x, y) | ValDef (x, y) <- defs]
        dataBinds = foldl' (.) id (dataToLet <$> [d | DataDef d <- defs])
    in
      dataBinds $ Let $ U.bind (rec binds) (Var (V (string2Name "main") Prefix))

translateEnv :: Env p -> Env q
translateEnv = unsafeCoerce

programToEnv :: Program p -> Env FlatPattern
programToEnv (Program defs)
  = let dataDefs = [d | DataDef d <- defs]
    in
     translateEnv $ foldl' (.) id (dataToEnv <$> dataDefs) $ mempty

runFile :: MonadIO m =>
           FilePath
        -> m (Either (ParseErrorBundle Text Void)
                     (Either (ParseErrorBundle TushTokenStream Void)
                             (Exp FlatPattern)))
runFile fp = do
  programText <- readFileUtf8 fp
  return $ fmap (eval . fst) <$> programTush programText

programTush :: Text
            -> (Either (ParseErrorBundle Text Void)
                       (Either (ParseErrorBundle TushTokenStream Void)
                               (Exp FlatPattern, Env FlatPattern)))
programTush pt = fmap (\x -> (runFreshM $ flattenPatterns $ programToExp x, programToEnv x)) <$> (parseTush programP pt)

checkTushProgram :: Text
                 -> Either (ParseErrorBundle Text Void)
                           (Either (ParseErrorBundle TushTokenStream Void)
                                   (Either (TypeError FlatPattern)
                                            Scheme))
checkTushProgram text_ = fmap (\x -> inferExp (programToEnv x) $ U.runFreshM $ flattenPatterns $ programToExp x) <$> (parseTush programP text_)

testProgramTush :: MonadIO m => Text -> m ()
testProgramTush text_ = case programTush text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right lexed -> case lexed of
    Left e -> putStr $ pack $ errorBundlePretty e
    Right (parsed, env) -> case inferExp env parsed of
      Left e -> do
        print e
        putStrLn $ runFreshM $ pExp pFlatPattern $ eval parsed
      Right scheme -> do
        putStrLn $ prettyPrintScheme scheme
        putStrLn $ runFreshM $ pExp pFlatPattern $ eval parsed

simpleConstructor :: Exp FlatPattern
simpleConstructor = Let $ U.bind (rec [(FPName $ s2n "A", Embed $ Lam $ U.bind (FPName $ s2n "x") (Lit $ LObject $ Object (s2n "A") (s2n "A") [Var $ V (s2n "x") Prefix]))]) (Var $ V (s2n "A") Prefix)

weirdProgram :: Either (ParseErrorBundle Text Void)
                       (Either (ParseErrorBundle TushTokenStream Void)
                               (Exp FlatPattern))
weirdProgram = fmap (eval . runFreshM . flattenPatterns . programToExp) <$> (parseTush programP $ pack "fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1))\n\nx = 5\n\nmain = fact x\n")
