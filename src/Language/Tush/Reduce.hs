{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Tush.Reduce where

import Language.Tush.Types
import Language.Tush.Pretty

import Unbound.Generics.LocallyNameless
import Unsafe.Coerce

import ClassyPrelude

translateName :: Name a -> Name b
translateName = unsafeCoerce

flattenPatterns :: Fresh m => Exp Pattern -> m (Exp FlatPattern)
flattenPatterns (Var (V n f)) = return $ Var (V (translateName n) f)
flattenPatterns (App e1 e2) = App <$> (flattenPatterns e1) <*> (flattenPatterns e2)
flattenPatterns (Lam b) = do
  (pat, body) <- unbind b
  body' <- flattenPatterns body
  case pat of
    PName x -> do
      return $ Lam $ bind (FPName $ translateName x) body'
    c@PConstructor{} -> do
      dummyVar <- fresh $ string2Name "x"
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
