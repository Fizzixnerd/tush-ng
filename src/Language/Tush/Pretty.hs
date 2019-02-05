{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Pretty where

import Language.Tush.Types

import ClassyPrelude

import Unbound.Generics.LocallyNameless

parens :: Text -> Text
parens x = "(" ++ x ++ ")"

pExp :: Fresh m => Exp -> m Text
pExp (Var (V name fixity))
  = case fixity of
      InfixBackticks -> return $ pack $ "`" ++ name2String name ++ "`"
      _ -> return $ pack $ name2String name
pExp (App e1@(Var (V _ fixity)) e2)
  = case fixity of
      Infix -> do
        pe1 <- pExp e1
        pe2 <- pExp e2
        return $ pe2 ++ " " ++ pe1
      InfixBackticks -> do
        pe1 <- pExp e1
        pe2 <- pExp e2
        return $ pe2 ++ " " ++ pe1
      Prefix -> do
        pe1 <- pExp e1
        pe2 <- pExp e2
        return $ pe1 ++ " " ++ pe2
pExp (App e1 e2) = do
  pe1 <- pExp e1
  pe2 <- pExp e2
  return $ pe1 ++ " " ++ pe2
pExp (Lam b) = do
  (name, e) <- unbind b
  pe <- pExp e
  return $ parens $ "λ " ++ (pack $ name2String name) ++ " → " ++ pe
pExp (Let b e2) = do
  (name, e1) <- unbind b
  pe1 <- pExp e1
  pe2 <- pExp e2
  return $ parens $ "let " ++ (pack $ name2String name) ++ " = " ++ pe1 ++ " in " ++ pe2
pExp (Lit l) = return $ pLit l
pExp (If cond tru fals) = do
  pc <- pExp cond
  pt <- pExp tru
  pf <- pExp fals
  return $ parens $ "if " ++ pc ++ " then " ++ pt ++ " else " ++ pf
pExp (Fix e) = do
  pe <- pExp e
  return $ "fix " ++ pe

pLit :: Lit -> Text
pLit (LBool b) = tshow b
pLit (LInt i) = tshow i
pLit (LFloat f) = tshow f
pLit (LPath p) = pPath p
pLit (LString s) = tshow s
pLit (LChar c) = tshow c

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

