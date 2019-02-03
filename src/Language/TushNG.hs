{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Language.TushNG where

import ClassyPrelude as CP hiding (TVar)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS
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

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype Infer a
  = Infer { unInfer :: ExceptT TypeError (RWST Env (Vector Constraint) InferState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadWriter (Vector Constraint)
           , MonadState InferState
           , MonadRWS Env (Vector Constraint) InferState
           , MonadError TypeError
           )

data InferState = InferState { count :: Int }
  deriving (Eq, Ord, Show)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  | Ambiguous (Vector Constraint)
  | UnificationMismatch (Vector Type) (Vector Type)
  deriving (Eq, Ord, Show)

newtype Constraint = Constraint { unConstraint :: (Type, Type) }
  deriving (Eq, Ord, Show)

newtype Env = Env { unEnv :: Map Name Scheme }
  deriving (Eq, Show)

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = mempty
  ftv (TVar a)       = singleton a
  ftv (t1 `TArr` t2) = ftv t1 `union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
    where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t \\ setFromList as

instance Substitutable Constraint where
   apply s (Constraint (t1, t2)) = Constraint (apply s t1, apply s t2)
   ftv (Constraint (t1, t2)) = ftv t1 `union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (union . ftv) mempty

instance Substitutable a => Substitutable (Vector a) where
  apply = fmap . apply
  ftv = foldr (union . ftv) mempty

instance Substitutable Env where
  apply s (Env env) = Env $ fmap (apply s) env
  ftv (Env env) = ftv $ Map.elems env

newtype Unifier = Unifier { unUnifier :: (Subst, Vector Constraint) }
  deriving (Eq, Ord, Show)

newtype Solve a = Solver { unSolve :: ExceptT TypeError (StateT Unifier Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError TypeError
           , MonadState Unifier
           )

-- Types

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

-- Env

extend :: Env -> Name -> Scheme -> Env
extend (Env env) x s = Env $ insertMap x s env

remove :: Env -> Name  -> Env
remove (Env env) name = Env $ deleteMap name env

extends :: Env -> [(Name, Scheme)] -> Env
extends (Env env) xs = Env $ union (mapFromList xs) env

lookup :: Name -> Env -> Maybe Scheme
lookup key (Env env) = CP.lookup key env

merge :: Env -> Env -> Env
merge (Env e1) (Env e2) = Env $ union e1 e2

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge mempty

singletonEnv :: Name -> Scheme -> Env
singletonEnv name scheme = Env $ singletonMap name scheme

keysEnv :: Env -> [Name]
keysEnv (Env env) = keys env

envFromList :: [(Name, Scheme)] -> Env
envFromList xs = Env $ mapFromList xs

envToList :: Env -> [(Name, Scheme)]
envToList (Env env) = mapToList env

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = Env mempty

-- Infer

initInfer :: InferState
initInfer = InferState { count = 0 }

runInfer :: Env -> Infer Type -> (Either TypeError Type, InferState, Vector Constraint)
runInfer env (Infer m) = runIdentity $ runRWST (runExceptT m) env initInfer

-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [Constraint (t1, t2)]

-- | Extend type environment
inEnv :: Name -> Scheme -> Infer a -> Infer a
inEnv x sc m = do
  let scope e = extend (remove e x) x sc
  local scope m
.
inferExp :: Env -> Exp -> Either TypeError Scheme
inferExp env e
  = let (t, s, w) = runInfer env (infer e)
    in
      case t of
        Left err -> Left err
        Right ty -> case runSolve w of
          Left err -> Left err
          Right subst -> Right $ closeOver $ apply subst ty

-- | Infer type of expression
infer :: Exp -> Infer Type
infer e = case e of
  Lit (LInt _) -> return typeInt
  Lit (LBool _) -> return typeBool
  Var x -> lookupEnv x
  Lam x e -> do
    tv <- fresh
    t <- inEnv x (Forall [] tv) (infer e)
    return $ tv `TArr` t
  App e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh
    uni t1 (t2 `TArr` tv)
    return tv
  Let x e1 e2 -> do
    env <- ask
    t1 <- infer e1
    let sc = generalize env t1
    t2 <- inEnv x sc (infer e2)
    return t2
  Fix e' -> do
    t1 <- infer e'
    tv <- fresh
    uni t1 (tv `TArr` tv)
    return tv
  Op op e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` t2 `TArr` tv
        u2 = ops Map.! op
    uni u1 u2
    return tv
  If cond tru fals -> do
    t1 <- infer cond
    t2 <- infer tru
    t3 <- infer fals
    uni t1 typeBool
    uni t2 t3
    return t2

-- Solve

emptyUnifier :: Unifier
emptyUnifier = Unifier (mempty, mempty)

unifies :: Type -> Type -> Solve Unifier
unifies t1 t2 | t1 == t2 = return emptyUnifier
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
  Unifier (su1, cs1) <- unifies t1 t2
  Unifier (su2, cs2) <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ Unifier (su2 `compose` su1, cs1 <> cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch (fromList t1) (fromList t2)

solver :: Solve Subst
solver = do
  Unifier (su, cs) <- get
  case uncons cs of
    Nothing -> return su
    Just (Constraint (t1, t2), rest) -> do
      Unifier (su1, cs1) <- unifies t1 t2
      put $ Unifier (su1 `compose` su, cs1 <> (apply su1 rest))
      solver

runSolve :: Unifier -> Either TypeError Subst
runSolve unifier = fst $ runIdentity $ runStateT (runExceptT $ unSolve solver) unifier

bind :: TVar -> Type -> Solve Unifier
bind a t | t == TVar a = return emptyUnifier
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Unifier ((Subst $ singletonMap a t), [Constraint (TVar a, t)])

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `member` ftv t

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `union` s1
