{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Pretty where

import Language.Tush.Types

import ClassyPrelude

import Unbound.Generics.LocallyNameless

parens :: Text -> Text
parens x = "(" ++ x ++ ")"

pExp :: (Fresh m, Alpha p, Typeable p) => (p -> Text) -> (Exp p) -> m Text
pExp _ (Builtin b) = return $ pBuiltin b
pExp _ (Var (V name fixity))
  = case fixity of
      InfixBackticks -> return $ pack $ "`" ++ name2String name ++ "`"
      _ -> return $ pack $ name2String name
pExp pp (App e1@(Var (V _ fixity)) e2)
  = case fixity of
      Infix -> do
        pe1 <- pExp pp e1
        pe2 <- pExp pp e2
        return $ pe2 ++ " " ++ pe1
      InfixBackticks -> do
        pe1 <- pExp pp e1
        pe2 <- pExp pp e2
        return $ pe2 ++ " " ++ pe1
      Prefix -> do
        pe1 <- pExp pp e1
        pe2 <- pExp pp e2
        return $ pe1 ++ " " ++ pe2
pExp pp (App e1 e2) = do
  pe1 <- pExp pp e1
  pe2 <- pExp pp e2
  return $ pe1 ++ " " ++ pe2
pExp pp (Lam b) = do
  (name, e) <- unbind b
  pe <- pExp pp e
  return $ parens $ "λ " ++ pp name ++ " → " ++ pe
pExp pp (Let binds) = do
  (r, body) <- unbind binds
  let bindings = unrec r
  pBindings <- concat . intersperse "; " <$> (mapM (\(p, Embed e) -> do
                                                       e' <- pExp pp e
                                                       return $ pp p ++ " = " ++ e') bindings)
  pBody <- pExp pp body
  return $ parens $ "let " ++ pBindings ++ " in " ++ pBody
pExp pp (Lit l) = pLit pp l
pExp pp (If cond tru fals) = do
  pc <- pExp pp cond
  pt <- pExp pp tru
  pf <- pExp pp fals
  return $ parens $ "if " ++ pc ++ " then " ++ pt ++ " else " ++ pf

pLit :: (Fresh m, Alpha p, Typeable p) => (p -> Text) -> Lit p -> m Text
pLit _ (LBool b) = return $ tshow b
pLit _ (LInt i) = return $ tshow i
pLit _ (LFloat f) = return $ tshow f
pLit _ (LPath p) = return $ pPath p
pLit _ (LString s) = return $ tshow s
pLit _ (LChar c) = return $ tshow c
pLit pp (LObject (Object _ name vals)) = do
  pVals <- concat . intersperse " " <$> (mapM (pExp pp) vals)
  return $ (pack $ name2String name) ++ " " ++ pVals

pPath :: Path -> Text
pPath (Path (bdy, pathType, fileType))
  = let prefix = case pathType of
          PAbs -> ""
          PRel -> "."
          PExec -> "!"
          PHome -> "~"
        postfix = case fileType of
          FTRegular -> ""
          FTDirectory -> "/"
    in
      pack $ prefix ++ "/" ++ (concat $ intersperse "/" bdy) ++ postfix

pBuiltin :: Builtin -> Text
pBuiltin b = parens $ "builtin " ++ (toLower $ tshow b)

pPattern :: Pattern -> Text
pPattern (PName n) = pack $ name2String n
pPattern (PConstructor constructor subpats) = parens $ (pack $ name2String constructor) ++ " " ++ (concat $ intersperse " " (pPattern <$> subpats))

pFlatPattern :: FlatPattern -> Text
pFlatPattern (FPName n) = pack $ name2String n
pFlatPattern (FPConstructor c names) = parens $ (pack $ name2String c) ++ " " ++ (concat $ intersperse " " (pack . name2String <$> names))

pPlainName :: PlainName -> Text
pPlainName (PlainName n) = pack $ name2String n
