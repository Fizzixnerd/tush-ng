{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Tush.Reduce where

import Language.Tush.Types
import Language.Tush.Pretty

import Data.List.Index (imap)

import Unbound.Generics.LocallyNameless
import Unsafe.Coerce

import ClassyPrelude

translateName :: Name a -> Name b
translateName = unsafeCoerce

preTypechecking :: Fresh m => Exp Pattern -> m (Exp FlatPattern)
preTypechecking = flattenPatterns

flattenPatterns :: Fresh m => Exp Pattern -> m (Exp FlatPattern)
flattenPatterns (Var (V n f)) = return $ Var (V (translateName n) f)
flattenPatterns (App e1 e2) = App <$> (flattenPatterns e1) <*> (flattenPatterns e2)
flattenPatterns (Lam b) = do
  (pat, body) <- unbind b
  body' <- flattenPatterns body
  case pat of
    PName name -> do
      return $ Lam $ bind (FPName $ translateName name) body'
    c@(PConstructor consName _) -> do
      dummyVar <- fresh $ s2n $ name2String consName ++ "'"
      flattenPatterns (Lam $ bind (PName dummyVar) (Let $ bind (rec [(c, Embed $ Var $ V dummyVar Prefix)]) body))
flattenPatterns (Let bs) = do
  (pats, body) <- unbind bs
  let bindings = (\(x, Embed y) -> (x, y)) <$> unrec pats
  flatBinds <- concat <$> (mapM patternBindToFlattenedBinds bindings)
  let embeddedFlatBinds = (\(x, y) -> (x, Embed y)) <$> flatBinds
  flatBody <- flattenPatterns body
  return $ Let $ bind (rec embeddedFlatBinds) flatBody
flattenPatterns (Lit (LInt x)) = return (Lit (LInt x))
flattenPatterns (Lit (LFloat x)) = return (Lit (LFloat x))
flattenPatterns (Lit (LPath x)) = return (Lit (LPath x))
flattenPatterns (Lit (LString x)) = return (Lit (LString x))
flattenPatterns (Lit (LChar x)) = return (Lit (LChar x))
flattenPatterns (Lit (LBool x)) = return (Lit (LBool x))
flattenPatterns (Lit (LObject (Object ty tag contents))) = do
  contents' <- mapM flattenPatterns contents
  return $ Lit (LObject (Object ty (translateName tag) contents'))
flattenPatterns (If cond tru fals) = do
  cond' <- flattenPatterns cond
  tru' <- flattenPatterns tru
  fals' <- flattenPatterns fals
  return $ If cond' tru' fals'
flattenPatterns (Builtin b) = return $ Builtin b

postTypeChecking :: Fresh m => Exp FlatPattern -> m (Exp PlainName)
postTypeChecking = removePatterns

removePatterns :: Fresh m => Exp FlatPattern -> m (Exp PlainName)
removePatterns (Var (V n f)) = return $ Var (V (translateName n) f)
removePatterns (App e1 e2) = App <$> (removePatterns e1) <*> (removePatterns e2)
removePatterns (Lam b) = do
  (pat, body) <- unbind b
  body' <- removePatterns body
  dummyVar <- friendlyDummy pat
  let unsafeNthNames = unsafeNthify dummyVar pat
  return $ Lam $ bind (PlainName dummyVar) (Let $ bind (rec unsafeNthNames) body')
removePatterns (Let bs) = do
  (pats, body) <- unbind bs
  body' <- removePatterns body
  let bindings = unrec pats
      names = fmap translateName . flatPatternNames . fst <$> bindings
  dummyBindings <- mapM (\((pat, Embed e), name) -> do
                            dummy <- friendlyDummy pat
                            e' <- removePatterns e
                            return (dummy, Embed e')) (zip bindings names)
  let dummyVars = fst <$> dummyBindings
      unsafeNthNames = concat $ uncurry unsafeNthify <$> (zip dummyVars $ fst <$> bindings)
  return $ Let $ bind (rec $ ((\(x, y) -> (PlainName x, y)) <$> dummyBindings) <> unsafeNthNames) body'
removePatterns (Lit (LInt x)) = return (Lit (LInt x))
removePatterns (Lit (LFloat x)) = return (Lit (LFloat x))
removePatterns (Lit (LPath x)) = return (Lit (LPath x))
removePatterns (Lit (LString x)) = return (Lit (LString x))
removePatterns (Lit (LChar x)) = return (Lit (LChar x))
removePatterns (Lit (LBool x)) = return (Lit (LBool x))
removePatterns (Lit (LObject (Object ty tag contents))) = do
  contents' <- mapM removePatterns contents
  return $ Lit (LObject (Object ty (translateName tag) contents'))
removePatterns (If cond tru fals) = do
  cond' <- removePatterns cond
  tru' <- removePatterns tru
  fals' <- removePatterns fals
  return $ If cond' tru' fals'
removePatterns (Builtin b) = return $ Builtin b

friendlyDummy :: Fresh m => FlatPattern -> m (Name (Exp p))
friendlyDummy (FPName name) = fresh $ s2n $ name2String name ++ "'"
friendlyDummy (FPConstructor consName _) = fresh $ s2n $ toLower $ name2String consName ++ "'"

unsafeNthify :: Name (Exp p) -> FlatPattern -> [(PlainName, Embed (Exp p))]
unsafeNthify dummyVar (FPConstructor _ names) = imap (\idx name -> (PlainName $ translateName name, Embed $ App (App (Builtin ONth) (Lit $ LInt $ fromIntegral idx)) (Var $ V dummyVar Prefix))) names
unsafeNthify dummyVar (FPName name) = [(PlainName $ translateName name, Embed $ Var $ V dummyVar Prefix)]

flatPatternNames :: FlatPattern -> [Name (Exp FlatPattern)]
flatPatternNames (FPName n) = singleton n
flatPatternNames (FPConstructor _ names) = names

patternBindToFlattenedBinds :: Fresh m => (Pattern, Exp Pattern) -> m [(FlatPattern, Exp FlatPattern)]
patternBindToFlattenedBinds (pat, e) = do
  e' <- flattenPatterns e
  case pat of
    PName name -> return [(FPName $ translateName name, e')]
    PConstructor conName pats -> do
      dummyVars <- mapM (\case
                            PName x -> fresh $ string2Name $ name2String x ++ "'"
                            PConstructor c _ -> fresh $ string2Name $ toLower $ name2String c) pats
      let expressionizeVar name = Var $ V (translateName name) Prefix
          subpats = zip pats (expressionizeVar <$> dummyVars)
      rest <- mapM patternBindToFlattenedBinds subpats
      return $ (FPConstructor (translateName conName) (translateName <$> dummyVars), e') : concat rest

bindToTransform :: Fresh m => (Pattern, Exp Pattern) -> (Exp FlatPattern -> m (Exp FlatPattern))
bindToTransform (pat, e) = \body -> do
  e' <- flattenPatterns e
  case pat of
    PConstructor consName pats -> do
      dummyVars <- forM pats $ \case
        PName x -> fresh $ string2Name $ name2String x ++ "'"
        PConstructor x _ -> fresh $ string2Name $ toLower $ name2String x
      let expressionizeVar name = Var $ V (translateName name) Prefix
          subpats = zip pats (expressionizeVar <$> dummyVars)
          xform = foldl' (\acc subpat -> acc <=< (bindToTransform subpat)) return subpats
      body' <- xform body
      return $ Let $ bind (rec $ [(FPConstructor (translateName consName) dummyVars, Embed e')]) $ body'
    PName name -> do
      return $ Let $ bind (rec $ [(FPName $ translateName name, Embed e')]) body
