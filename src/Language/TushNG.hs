{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.TushNG where

import qualified Unbound.Generics.LocallyNameless as U

import Language.Tush.Types
import Language.Tush.Parse
import Language.Tush.Reduce
import Language.Tush.Result
import Language.Tush.Pretty

import ClassyPrelude as CP hiding (TVar)

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

binary :: Type -> Type
binary t = t `TArr` t `TArr` t

endo :: Type -> Type
endo t = t `TArr` t

typeBinaryInt :: Type
typeBinaryInt = binary typeInt

unArrow :: Type -> [Type]
unArrow t@TCon{} = [t]
unArrow (t `TArr` u) = t : unArrow u
unArrow t@TVar{} = [t]

arrow :: [Type] -> Type
arrow [] = error "`arrow' called on empty list."
arrow [ty] = ty
arrow (ty:tys) = ty `TArr` (arrow tys)

-- * Env
extend :: Env p -> U.Name (Exp p) -> Scheme -> Env p
extend (Env env) x s = Env $ insertMap x s env

remove :: Env p -> U.Name (Exp p) -> Env p
remove (Env env) name = Env $ deleteMap name env

extends :: Env p -> [(U.Name (Exp p), Scheme)] -> Env p
extends (Env env) xs = Env $ union (mapFromList xs) env

lookupName :: U.Name (Exp p) -> (Env p) -> Maybe Scheme
lookupName key (Env env) = CP.lookup key env

mergeEnvs :: [Env p] -> Env p
mergeEnvs = foldl' (<>) mempty

singletonEnv :: U.Name (Exp p) -> Scheme -> Env p
singletonEnv name scheme = Env $ singletonMap name scheme

keysEnv :: Env p -> [U.Name (Exp p)]
keysEnv (Env env) = keys env

envFromList :: [(U.Name (Exp p), Scheme)] -> Env p
envFromList xs = Env $ mapFromList xs

envToList :: Env p -> [(U.Name (Exp p), Scheme)]
envToList (Env env) = mapToList env

-- Infer

initInfer :: InferState
initInfer = InferState { count = 0 }

runInfer :: Env p -> Infer p (Type, Vector (Constraint p)) -> (Either (TypeError p) (Type, Vector (Constraint p)))
runInfer env (Infer m) = fst $ U.runFreshM $ runStateT (runReaderT (runExceptT m) env) initInfer

inferExp :: Env FlatPattern -> Exp FlatPattern -> Result Scheme
inferExp env e = case constraintsExp env e of
  Right (_,_,_, s) -> return s
  Left err -> throwError $ TypeErr err e

constraintsExp :: Env FlatPattern -> Exp FlatPattern -> Either (TypeError FlatPattern) (Vector (Constraint FlatPattern), Subst', Type, Scheme)
constraintsExp env e = case runInfer env (infer e) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, closeOver $ apply subst ty)

closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

inEnv :: U.Name (Exp p) -> Scheme -> Infer p a -> Infer p a
inEnv name scheme m = do
  let scope e = extend (remove e name) name scheme
  local scope m

inEnv' :: [(U.Name (Exp p), Scheme)] -> Infer p a -> Infer p a
inEnv' types m = do
  let scope e = extends e types
  local scope m

lookupEnv :: U.Name (Exp p) -> Infer p Type
lookupEnv name = do
  env <- ask
  case lookupName name env of
    Nothing -> throwError $ UnboundVariable name --fresh -- This used to throw an UnboundVariable Exception.
    Just s -> do
      t <- instantiate s
      return t

letters :: [String]
letters = [1..] >>= flip CP.replicateM ['a'..'z']

fresh :: Infer p Type
fresh = do
  s <- get
  put s { count = count s + 1}
  return $ TVar $ TV (letters `unsafeIndex` count s)

instantiate :: Scheme -> Infer p Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst' $ mapFromList $ zip as as'
  return $ apply s t

generalize :: Env p -> Type -> Scheme
generalize env t = Forall as t
  where as = toList $ ftv t \\ ftv env

inPattern :: FlatPattern -> Infer FlatPattern a -> Infer FlatPattern a
inPattern (FPName name) action = do
  freshName <- fresh
  let scheme = Forall [] freshName
  result <- inEnv name scheme action
  return result
inPattern pat@(FPConstructor (ConstructorName consName) names) action = do
  consType <- lookupEnv $ U.s2n consName
  env <- ask
  let consTypes = unsafeInit $ unArrow consType
  if length names == length consTypes
    then do
    let consSchemes = generalize env <$> consTypes
        nameTypePairs = zip names consSchemes
    result <- inEnv' nameTypePairs action
    return result
    else throwError $ UnificationMismatch pat consType

patternNames :: FlatPattern -> [U.Name (Exp FlatPattern)]
patternNames (FPName n) = singleton n
patternNames (FPConstructor _ names) = names

-- | Infer type of expression
infer :: Exp FlatPattern -> Infer FlatPattern (Type, Vector (Constraint FlatPattern))
infer e = case e of
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])
  Lit (LPath _) -> return (typePath, [])
  Lit (LFloat _) -> return (typeFloat, [])
  Lit (LChar _ ) -> return (typeChar, [])
  Lit (LString _) -> return (typeString, [])
  Lit (LObject (Object typeName _ _)) -> do
    let consTy = TCon $ Name' $ pack typeName
    return (consTy, [])
  Builtin b -> case b of
    IAdd -> return (binary typeInt, [])
    ISub -> return (binary typeInt, [])
    IMul -> return (binary typeInt, [])
    IDiv -> return (binary typeInt, [])
    IRem -> return (binary typeInt, [])
    IEql -> return (TCon "Int" `TArr` TCon "Int" `TArr` TCon "Bool", [])
    INeq -> return (TCon "Int" `TArr` TCon "Int" `TArr` TCon "Bool", [])
    BNot -> return (endo typeBool, [])
    BXor -> return (binary typeBool, [])
    ONth -> error "unreachable: ONth should never occur before typechecking!"
  Var (V x _) -> do
    t <- lookupEnv x
    return (t, [])
  Lam b -> do
    (pat, e') <- U.unbind b
    inPattern pat $ do
      (bodyType, bodyConstraints) <- (infer e')
      case pat of
        FPName name -> do
          tv <- lookupEnv name
          return (tv `TArr` bodyType, bodyConstraints)
        FPConstructor (ConstructorName consName) _ -> do
          consType <- unsafeLast . unArrow <$> (lookupEnv $ U.s2n consName)
          return (consType `TArr` bodyType, bodyConstraints)
  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 <> c2 <> [Constraint ((t1, e1), (t2 `TArr` tv, e))])
  Let binds -> do
    (r, body) <- U.unbind binds
    let bindings = (\(x, U.Embed y) -> (x, y)) <$> U.unrec r
        makeFreshType name = do
          freshVar <- fresh
          return (name, freshVar)
    freshNames <- mapM makeFreshType (concat $ patternNames . fst <$> bindings)
    generalizedFreshNames <- mapM (\(n, t) -> do
                                      env <- ask
                                      return (n, generalize env t)) freshNames
    inEnv' generalizedFreshNames $ do
      env <- ask
      (env', patConstraints) <- CP.foldM patternConstraints (env, []) bindings
      local (const env') $ do
        case runSolve patConstraints of
          Left err -> throwError err
          Right sub -> do
            (bodyType, bodyConstraints) <- local (apply sub) (infer body)
            let constraints = bodyConstraints <> patConstraints
            return (bodyType, constraints)
  Dat bs -> do
    (binds, body) <- U.unbind bs
    inEnv' ((\(name, _, scheme) -> (name, scheme)) <$> U.unrec binds) (infer body)
  If cond tru fals -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tru
    (t3, c3) <- infer fals
    return (t2, c1 <> c2 <> c3 <> [Constraint ((t1, cond), (typeBool, e)), Constraint ((t2, tru), (t3, fals))])

patternConstraints :: (Env FlatPattern, Vector (Constraint FlatPattern))
                   -> (FlatPattern, Exp FlatPattern)
                   -> Infer FlatPattern (Env FlatPattern, Vector (Constraint FlatPattern))
patternConstraints (env, cs) (pat, e) = local (const env) $ inPattern pat $ do
  (rhsType, rhsConstraints) <- infer e
  case pat of
    FPName name -> do
      varType <- lookupEnv name
      newEnv <- ask
      return (newEnv, cs <> rhsConstraints <> [Constraint ((rhsType, e), (varType, Var $ V name Prefix))])
    FPConstructor (ConstructorName consName) names -> do
      consTypes <- unArrow <$> (lookupEnv $ U.s2n consName)
      let consValType = unsafeLast consTypes
          constraints = cs
                        <> rhsConstraints
                        <> [Constraint ((rhsType, e), (consValType, Var $ V (U.s2n $ consName ++ " @ ") Prefix))]
      newEnv <- ask
      return (newEnv, constraints)

inferTop :: Env FlatPattern -> Vector (U.Name (Exp FlatPattern), (Exp FlatPattern)) -> Result (Env FlatPattern)
inferTop env xs = case uncons xs of
  Nothing -> Right env
  Just ((name, e), rest) -> do
    ty <- inferExp env e
    inferTop (extend env name ty) rest

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

unifies :: Constraint p -> Solve p Subst'
unifies (Constraint ((t1, _), (t2, _))) | t1 == t2 = return mempty
unifies c@(Constraint (((TVar v), _), (t, _))) = bind v t c
unifies c@(Constraint ((t, _), ((TVar v), _))) = bind v t c
unifies (Constraint (((TArr t1 t2), e1), ((TArr t3 t4), e2)))
  = unifyMany [ Constraint ((t1, e1), (t3, e2))
              , Constraint ((t2, e1), (t4, e2))
              ]
unifies c = throwError $ UnificationFail c

unifyMany :: [Constraint p] -> Solve p Subst'
unifyMany [] = return mempty
unifyMany (c : cs) = do
  su1 <- unifies c
  su2 <- unifyMany (apply su1 cs)
  return $ su2 `compose` su1

solver :: Unifier p -> Solve p Subst'
solver (Unifier (su, cs)) = do
  case uncons cs of
    Nothing -> return su
    Just (c, rest) -> do
      su1 <- unifies c
      solver $ Unifier (su1 `compose` su, apply su1 rest)

runSolve :: Vector (Constraint p) -> Either (TypeError p) Subst'
runSolve constraints = runExcept $ unSolve $ solver $ Unifier (mempty, constraints)

bind :: TVar -> Type -> Constraint p -> Solve p Subst'
bind a t c | t == TVar a = return mempty
           | occursCheck a t = throwError $ InfiniteType a t c
           | otherwise = return $ Subst' $ singletonMap a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `member` ftv t

compose :: Subst' -> Subst' -> Subst'
(Subst' s1) `compose` (Subst' s2) = Subst' $ fmap (apply (Subst' s1)) s2 `union` s1

checkTush :: Env FlatPattern
          -> Text
          -> Result (Scheme, Exp FlatPattern)
checkTush env text_ =
  let parsed = parseTush expP text_
      flattened = U.runFreshM . flattenPatterns <$> parsed
  in
    case flattened of
      Left e -> throwError e
      Right f -> do
        ty <- inferExp env f
        return (ty, f)

testCheckTush :: Env FlatPattern -> Text -> IO ()
testCheckTush env text_ = putStrLn $ prettyResult pScheme $ fst <$> (checkTush env text_)
