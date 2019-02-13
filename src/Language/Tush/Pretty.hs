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
pLit _ (LObject (Object _ (ConstructorName name) [])) = return $ pack name
pLit pp (LObject (Object _ (ConstructorName name) vals)) = do
  pVals <- concat . intersperse " " <$> (mapM (pExp pp) vals)
  return $ pack name ++ " @ " ++ pVals

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
pPattern (PConstructor (ConstructorName c) subpats) = parens $ (pack c) ++ " " ++ (concat $ intersperse " " (pPattern <$> subpats))

pFlatPattern :: FlatPattern -> Text
pFlatPattern (FPName n) = pack $ name2String n
pFlatPattern (FPConstructor (ConstructorName c) names) = parens $ (pack c) ++ " " ++ (concat $ intersperse " " (pack . name2String <$> names))

pPlainName :: PlainName -> Text
pPlainName (PlainName n) = pack $ name2String n

pTypeError :: (Fresh m, Alpha p, Typeable p) => (p -> Text) -> TypeError p -> m Text
pTypeError pp e = case e of
  UnificationFail c -> do
    constraint <- pConstraint pp c
    return $ "Could not unify, in constraint: " ++ constraint
  InfiniteType _ _ c -> do
    constraint <- pConstraint pp c
    return $ "Could not construct infinite type, in constraint: " ++ constraint
  UnboundVariable name -> return $ "Unbound variable: " ++ (pack $ name2String name)
  Ambiguous cs -> do
    constraints <- sequence $ intersperse (return ", ") $ (pConstraint pp <$> cs)
    return $ "Ambiguous type arising from constraints: " ++ concat constraints
  UnificationMismatch p ty -> return $ "Unification mismatch: in pattern: " ++ pp p ++ " whose constructor has type: " ++ pType ty ++ ". Did you get the number of arguments right?"


pConstraint :: (Fresh m, Alpha p, Typeable p) =>(p -> Text) -> Constraint p -> m Text
pConstraint pp (Constraint ((t1, e1), (t2, e2)))
  = do
  let pt1 = pType t1
  let pt2 = pType t2
  pe1 <- pExp pp e1
  pe2 <- pExp pp e2
  return $ pt1 ++ " ~ " ++ pt2 ++ " [in expressions: " ++ pe1 ++ " and " ++ pe2 ++ "]"

pType :: Type -> Text
pType (TVar (TV x)) = pack x
pType (TArr t1 t2) = "(" ++ pType t1 ++ " → " ++ pType t2 ++ ")"
pType (TCon (Name' x)) = pack x

pScheme :: Scheme -> Text
pScheme (Forall tvs t)
  = if null tvs
    then pType t
    else "∀ " ++ concat (intersperse ", " ((\(TV x) -> pack x) <$> tvs)) ++ ". " ++ pType t

