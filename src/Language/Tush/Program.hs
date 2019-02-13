{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Language.Tush.Result
import Language.TushNG

programToExp :: Program Pattern -> Exp Pattern
programToExp (Program defs)
  = let binds = [(PName x, y) | ValDef (x, y) <- defs]
        dataBinds = foldl' (++) [] (dataToDataLet <$> [d | DataDef d <- defs])
    in
      Dat $ U.bind (rec dataBinds) $ Let $ U.bind (rec binds) (Var (V (string2Name "main") Prefix))

translateEnv :: Env p -> Env q
translateEnv = unsafeCoerce

-- programToEnv :: Program p -> Env FlatPattern
-- programToEnv (Program defs)
--   = let dataDefs = [d | DataDef d <- defs]
--     in
--      translateEnv $ foldl' (.) id (dataToEnv <$> dataDefs) $ mempty

runFile :: MonadIO m =>
           FilePath
        -> m (Result (Exp PlainName, Scheme))
runFile fp = runFreshM <$> runFileM fp

runFileM :: (MonadIO m, Fresh f) =>
            FilePath
         -> m (f (Result (Exp PlainName, Scheme)))
runFileM fp = fmap (fmap (fmap (\(e, s) -> (eval e, s)))) $ do
  programText <- readFileUtf8 fp
  return $ programTushM programText

programTush :: Text
            -> Result (Exp PlainName, Scheme)
programTush text_ = runFreshM $ programTushM text_

programTushM :: Fresh m =>
                Text
             -> m (Result (Exp PlainName, Scheme))
programTushM text_ = do
  let eProg = parseTush programP text_
  eFlat <- sequence $ flattenPatterns . programToExp <$> eProg
  ePlain <- sequence $ removePatterns <$> eFlat
  eScheme <- join <$> (sequence $ checkProgramM <$> eProg)
  return $ do
    ex <- ePlain
    sc <- eScheme
    return (ex, sc)

checkProgram :: Program Pattern -> Result Scheme
checkProgram p = inferExp mempty $ runFreshM $ flattenPatterns $ programToExp p

checkProgramM :: Fresh m => Program Pattern -> m (Result Scheme)
checkProgramM p = inferExp mempty <$> (flattenPatterns $ programToExp p)

checkTushProgram :: Text
                 -> Result Scheme
checkTushProgram text_ = join $ (\x -> inferExp mempty $ U.runFreshM $ flattenPatterns $ programToExp x) <$> (parseTush programP text_)

testProgramTush :: MonadIO m => Text -> m ()
testProgramTush text_ = putStrLn $ prettyResult (\(e, s) -> pScheme s ++ "\n" ++ runFreshM (pExp pPlainName e)) (programTush text_)

simpleConstructor :: Exp FlatPattern
simpleConstructor = Let $ U.bind (rec [(FPName $ s2n "A", Embed $ Lam $ U.bind (FPName $ s2n "x") (Lit $ LObject $ Object "A" (ConstructorName "A") [Var $ V (s2n "x") Prefix]))]) (Var $ V (s2n "A") Prefix)

testStepTush :: Text -> IO ()
testStepTush text_ = case programTush text_ of
    Left e -> putStrLn $ prettyTushError e
    Right (e, s) -> do
      putStrLn $ pScheme s
      showSteps e

testStepTushRaw :: Text -> IO ()
testStepTushRaw text_ = case programTush text_ of
    Left e -> putStrLn $ prettyTushError e
    Right (e, s) -> do
      putStrLn $ pScheme s
      showStepsRaw e

factProgram :: Result (Exp PlainName)
factProgram = (eval . runFreshM . (removePatterns <=< flattenPatterns) . programToExp) <$> (parseTush programP $ pack "fact = \\n -> if builtin ieql n 1 then 1 else builtin imul n (fact (builtin isub n 1))\n\nx = 5\n\nmain = fact x\n")
