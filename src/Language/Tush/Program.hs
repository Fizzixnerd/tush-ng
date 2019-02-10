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
                             (Exp PlainName, Either (TypeError FlatPattern) Scheme)))
runFile fp = do
  result <- runFileM fp
  return $ fmap runFreshM <$> result

runFileM :: (MonadIO m, Fresh f) =>
            FilePath
         -> m (Either (ParseErrorBundle Text Void)
                      (Either (ParseErrorBundle TushTokenStream Void)
                             (f (Exp PlainName, Either (TypeError FlatPattern) Scheme))))
-- FIXME: Dear god.
runFileM fp = fmap (fmap (fmap (fmap (\(e, t) -> (eval e, t))))) $ do
  programText <- readFileUtf8 fp
  return $ programTushM programText

programTush :: Text
            -> (Either (ParseErrorBundle Text Void)
                       (Either (ParseErrorBundle TushTokenStream Void)
                               (Exp PlainName, Either (TypeError FlatPattern) Scheme)))
programTush pt = fmap (\x -> (runFreshM $ (removePatterns =<< (flattenPatterns $ programToExp x)), checkProgram x)) <$> (parseTush programP pt)

programTushM :: Fresh m =>
                Text
             -> (Either (ParseErrorBundle Text Void)
                        (Either (ParseErrorBundle TushTokenStream Void)
                                (m (Exp PlainName, Either (TypeError FlatPattern) Scheme))))
programTushM pt = do
  lexed <- parseTush programP pt
  return $ do
    parsed <- lexed
    return $ do
      let check = checkProgram parsed
      ex <- removePatterns =<< (flattenPatterns $ programToExp parsed)
      return (ex, check)

checkProgram :: Program Pattern -> Either (TypeError FlatPattern) Scheme
checkProgram p = inferExp mempty $ runFreshM $ flattenPatterns $ programToExp p

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
    Right (parsed, ty) -> case ty of
      Left e -> do
        print e
        putStrLn $ runFreshM $ pExp pPlainName $ eval parsed
      Right scheme -> do
        putStrLn $ prettyPrintScheme scheme
        putStrLn $ runFreshM $ pExp pPlainName $ eval parsed

simpleConstructor :: Exp FlatPattern
simpleConstructor = Let $ U.bind (rec [(FPName $ s2n "A", Embed $ Lam $ U.bind (FPName $ s2n "x") (Lit $ LObject $ Object (s2n "A") (s2n "A") [Var $ V (s2n "x") Prefix]))]) (Var $ V (s2n "A") Prefix)

testStepTush :: Text -> IO ()
testStepTush text_ = do
  case programTush text_ of
    Left e -> putStr $ pack $ errorBundlePretty e
    Right lexed -> case lexed of
      Left e -> putStr $ pack $ errorBundlePretty e
      Right (parsed, ety) -> case ety of
        Left e -> do
          print e
          showSteps parsed
        Right ty -> do
          putStrLn $ prettyPrintScheme ty
          showSteps parsed

testStepTushRaw :: Text -> IO ()
testStepTushRaw text_ = do
  case programTush text_ of
    Left e -> putStr $ pack $ errorBundlePretty e
    Right lexed -> case lexed of
      Left e -> putStr $ pack $ errorBundlePretty e
      Right (parsed, ety) -> case ety of
        Left e -> do
          print e
          showStepsRaw parsed
        Right ty -> do
          putStrLn $ prettyPrintScheme ty
          showStepsRaw parsed


weirdProgram :: Either (ParseErrorBundle Text Void)
                       (Either (ParseErrorBundle TushTokenStream Void)
                               (Exp PlainName))
weirdProgram = fmap (eval . runFreshM . (removePatterns <=< flattenPatterns) . programToExp) <$> (parseTush programP $ pack "fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1))\n\nx = 5\n\nmain = fact x\n")
