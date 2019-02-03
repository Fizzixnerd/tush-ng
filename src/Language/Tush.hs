{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush where

import ClassyPrelude hiding (TVar)
import Control.Monad.Except hiding (replicateM)
import Control.Monad.State hiding (replicateM)
import qualified Data.Map as Map


newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show, IsString)

data Exp
  = Var Name
  | App Exp Exp
  | Lam Name Exp
  | Let Name Exp Exp
  | Lit Lit
  | If Exp Exp Exp
  | Fix Exp
  | Op Binop Exp Exp
  deriving (Eq, Ord, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord, Show)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program (Vector Dec) Exp
  deriving (Eq, Ord, Show)

data Dec = Dec Name Exp
  deriving (Eq, Ord, Show)

newtype TVar = TV Text
  deriving (Eq, Ord, Show)

data Type
  = TVar TVar
  | TCon Name
  | TArr Type Type
  deriving (Eq, Ord, Show)

infixr `TArr`

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype TypeEnv = TypeEnv { unTypeEnv :: (Map Name Scheme) }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

newtype Unique = Unique { unUnique :: Int }
  deriving (Eq, Ord, Show)

initUnique :: Unique
initUnique = Unique 0

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  deriving (Eq, Ord, Show)

newtype Infer a = Infer { unInfer :: ExceptT TypeError (State Unique) a }
  deriving (Functor, Applicative, Monad, MonadState Unique, MonadError TypeError)

newtype Subst = Subst { unSubst :: Map TVar Type }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

extend :: TypeEnv -> Name -> Scheme -> TypeEnv
extend (TypeEnv env) n s = TypeEnv $ insertMap n s env

-- restrict :: TypeEnv -> Name -> TypeEnv
-- restrict (TypeEnv env) n = TypeEnv $ deleteMap n env

data PartialType = PartialType
  { ptSubtitution :: Subst
  , ptType :: Type
  } deriving (Eq, Ord, Show)

runInfer :: Infer PartialType -> Either TypeError Scheme
runInfer m = closeOver <$> (evalState (runExceptT $ unInfer m) initUnique)

closeOver :: PartialType -> Scheme
closeOver (PartialType sub ty) = normalize $ generalize mempty (sub `apply` ty)

typeof :: TypeEnv -> Name -> Maybe Scheme
typeof (TypeEnv env) name = lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TVar

instance Substitutable Type where
  apply _ (TCon a) = TCon a
  apply (Subst s) t@(TVar a) = findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv (TCon _) = mempty
  ftv (TVar a) = singleton a
  ftv (t1 `TArr` t2) = ftv t1 `union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply (Subst s') t
    where s' = foldr deleteMap s as
  ftv (Forall as t) = ftv t \\ setFromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (union . ftv) mempty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ apply s <$> env
  ftv (TypeEnv env) = ftv $ Map.elems env

compose :: Subst -> Subst -> Subst
s1'@(Subst s1) `compose` (Subst s2) = Subst $ (apply s1' <$> s2) `union` s1

letters :: [Text]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  (Unique s) <- get
  put $ Unique (s + 1)
  return $ TVar $ TV (letters `unsafeIndex` s)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `member` ftv t

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return mempty
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst $ singletonMap a t

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ mapFromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = toList $ ftv t \\ ftv env

lookupEnv :: TypeEnv -> Name -> Infer PartialType
lookupEnv (TypeEnv env) name = do
  case lookup name env of
    Nothing -> throwError $ UnboundVariable name
    Just s -> do
      t <- instantiate s
      return $ PartialType mempty t

infer :: TypeEnv -> Exp -> Infer PartialType
infer env e = case e of
  Var x -> lookupEnv env x
  Lam x e -> do
    tv <- fresh
    let env' = extend env x (Forall [] tv)
    PartialType s1 t1 <- infer env' e
    return $ PartialType s1 ((apply s1 tv) `TArr` t1)
  Let x e1 e2 -> do
    PartialType s1 t1 <- infer env e1
    let env' = apply s1 env
        t' = generalize env' t1
    PartialType s2 t2 <- infer (extend env' x t') e2
    return $ PartialType (s1 `compose` s2) t2
  If cond tru fals -> do
    PartialType s1 t1 <- infer env cond
    PartialType s2 t2 <- infer env tru
    PartialType s3 t3 <- infer env fals
    s4 <- unify t1 typeBool
    s5 <- unify t2 t3
    return $ PartialType (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1) (apply s5 t2)
  Fix e -> do
    PartialType s1 t <- infer env e
    tv <- fresh
    s2 <- unify (TArr tv tv) t
    return $ PartialType s2 (apply s1 tv)
  Op op e1 e2 -> do
    PartialType s1 t1 <- infer env e1
    PartialType s2 t2 <- infer env e2
    tv <- fresh
    s3 <- unify (t1 `TArr` t2 `TArr` tv) (ops Map.! op)
    return $ PartialType (s1 `compose` s2 `compose` s3) (apply s3 tv)
  Lit (LInt _) -> return $ PartialType mempty typeInt
  Lit (LBool _) -> return $ PartialType mempty typeBool

ops :: Map Binop Type
ops = mapFromList
  [ (Add, typeInt `TArr` typeInt `TArr` typeInt)
  , (Mul, typeInt `TArr` typeInt `TArr` typeInt)
  , (Sub, typeInt `TArr` typeInt `TArr` typeInt)
  , (Eql, typeInt `TArr` typeInt `TArr` typeBool)
  ]
