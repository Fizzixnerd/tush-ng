{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Language.TushNG where

import Language.Tush.Types
import Language.Tush.Parse

import ClassyPrelude as CP hiding (TVar)

import Text.Megaparsec hiding (count)
import Data.Void
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.List as List

-- Types

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

typePath :: Type
typePath = TCon "Path"

typeFloat :: Type
typeFloat = TCon "Float"

typeChar :: Type
typeChar = TCon "Char"

typeString :: Type
typeString = TCon "String"

-- Env

extend :: Env -> Name -> Scheme -> Env
extend (Env env) x s = Env $ insertMap x s env

remove :: Env -> Name  -> Env
remove (Env env) name = Env $ deleteMap name env

extends :: Env -> [(Name, Scheme)] -> Env
extends (Env env) xs = Env $ union (mapFromList xs) env

lookupName :: Name -> Env -> Maybe Scheme
lookupName key (Env env) = CP.lookup key env

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' (<>) mempty

singletonEnv :: Name -> Scheme -> Env
singletonEnv name scheme = Env $ singletonMap name scheme

keysEnv :: Env -> [Name]
keysEnv (Env env) = keys env

envFromList :: [(Name, Scheme)] -> Env
envFromList xs = Env $ mapFromList xs

envToList :: Env -> [(Name, Scheme)]
envToList (Env env) = mapToList env

-- Infer

initInfer :: InferState
initInfer = InferState { count = 0 }

runInfer :: Env -> Infer (Type, Vector Constraint) -> (Either TypeError (Type, Vector Constraint))
runInfer env (Infer m) = fst $ runIdentity $ runStateT (runReaderT (runExceptT m) env) initInfer

inferExp :: Env -> Exp -> Either TypeError Scheme
inferExp env e = do
  (_, _, _, s) <- constraintsExp env e
  return s

constraintsExp :: Env -> Exp -> Either TypeError (Vector Constraint, Subst, Type, Scheme)
constraintsExp env e = case runInfer env (infer e) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, closeOver $ apply subst ty)

closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

inEnv :: Name -> Scheme -> Infer a -> Infer a
inEnv name scheme m = do
  let scope e = extend (remove e name) name scheme
  local scope m

lookupEnv :: Name -> Infer Type
lookupEnv name = do
  env <- ask
  case lookupName name env of
    Nothing -> throwError $ UnboundVariable name
    Just s -> do
      t <- instantiate s
      return t

letters :: [Text]
letters = [1..] >>= flip CP.replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s { count = count s + 1}
  return $ TVar $ TV (letters `unsafeIndex` count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ mapFromList $ zip as as'
  return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where as = toList $ ftv t \\ ftv env

ops :: Binop -> Type
ops Add = typeInt `TArr` typeInt `TArr` typeInt
ops Mul = typeInt `TArr` typeInt `TArr` typeInt
ops Sub = typeInt `TArr` typeInt `TArr` typeInt
ops Eql = typeInt `TArr` typeInt `TArr` typeBool

-- | Infer type of expression
infer :: Exp -> Infer (Type, Vector Constraint)
infer e = case e of
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])
  Lit (LPath _) -> return (typePath, [])
  Lit (LFloat _) -> return (typeFloat, [])
  Lit (LChar _ ) -> return (typeChar, [])
  Lit (LString _) -> return (typeString, [])
  Var (V x _) -> do
    t <- lookupEnv x
    return (t, [])
  Lam x e' -> do
    tv <- fresh
    (t, c) <- inEnv x (Forall [] tv) (infer e')
    return $ (tv `TArr` t, c)
  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 <> c2 <> [Constraint (t1, t2 `TArr` tv)])
  Let name e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
      Left err -> throwError err
      Right sub -> do
        let scheme = generalize (apply sub env) (apply sub t1)
        (t2, c2) <- inEnv name scheme $ local (apply sub) (infer e2)
        return (t2, c1 <> c2)
  Fix e' -> do
    (t1, c1) <- infer e'
    tv <- fresh
    return (tv, c1 <> [Constraint (tv `TArr` tv, t1)])
  Op op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` t2 `TArr` tv
        u2 = ops op
    return (tv, c1 <> c2 <> [Constraint (u1, u2)])
  If cond tru fals -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tru
    (t3, c3) <- infer fals
    return (t2, c1 <> c2 <> c3 <> [Constraint (t1, typeBool), Constraint (t2, t3)])

inferTop :: Env -> Vector (Name, Exp) -> Either TypeError Env
inferTop env xs = case uncons xs of
  Nothing -> Right env
  Just ((name, e), rest) -> case inferExp env e of
    Left err -> Left err
    Right ty -> inferTop (extend env name ty) rest

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normType body)
  where
    ord = zip (List.nub $ fv body) (map TV letters)

    fv (TVar a) = [a] :: [TVar]
    fv (TArr a b) = fv a <> fv b
    fv (TCon _) = []

    normType (TArr a b) = TArr (normType a) (normType b)
    normType (TCon a) = TCon a
    normType (TVar a) =
      case CP.lookup a ord of
        Just x -> TVar x
        Nothing -> terror "type variable not in signature."

-- Solve

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch (fromList t1) (fromList t2)

solver :: Unifier -> Solve Subst
solver (Unifier (su, cs)) = do
  case uncons cs of
    Nothing -> return su
    Just (Constraint (t1, t2), rest) -> do
      su1 <- unifies t1 t2
      solver $ Unifier (su1 `compose` su, apply su1 rest)

runSolve :: Vector Constraint -> Either TypeError Subst
runSolve constraints = runExcept $ unSolve $ solver $ Unifier (mempty, constraints)

bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ singletonMap a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `member` ftv t

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `union` s1

checkTush :: Env
          -> Text
          -> Either (ParseErrorBundle Text Void)
                    (Either (ParseErrorBundle TushTokenStream Void)
                            (Either TypeError
                                    Scheme))
checkTush env text_ = fmap (inferExp env) <$> (parseTush expP text_)

testCheckTush :: Env -> Text -> IO ()
testCheckTush env text_ = case checkTush env text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right x -> case x of
    Left e' -> putStr $ pack $ errorBundlePretty e'
    Right y -> case y of
      Left e'' -> print e''
      Right z -> print z
