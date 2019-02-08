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

binary :: Type -> Type
binary t = t `TArr` t `TArr` t

endo :: Type -> Type
endo t = t `TArr` t

typeBinaryInt :: Type
typeBinaryInt = binary typeInt

prettyPrintType :: Type -> Text
prettyPrintType (TVar (TV x)) = x
prettyPrintType (TArr t1 t2) = "(" ++ prettyPrintType t1 ++ " → " ++ prettyPrintType t2 ++ ")"
prettyPrintType (TCon (Name' x)) = pack x

prettyPrintScheme :: Scheme -> Text
prettyPrintScheme (Forall tvs t)
  = if null tvs
    then prettyPrintType t
    else "∀ " ++ concat (intersperse ", " ((\(TV x) -> x) <$> tvs)) ++ ". " ++ prettyPrintType t

unArrow :: Type -> [Type]
unArrow t@TCon{} = [t]
unArrow (t `TArr` u) = t : unArrow u
unArrow t@TVar{} = [t]

-- Env

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

runInfer :: Env p -> Infer p (Type, Vector Constraint) -> (Either (TypeError p) (Type, Vector Constraint))
runInfer env (Infer m) = fst $ U.runFreshM $ runStateT (runReaderT (runExceptT m) env) initInfer

inferExp :: Env FlatPattern -> Exp FlatPattern -> Either (TypeError FlatPattern) Scheme
inferExp env e = do
  (_, _, _, s) <- constraintsExp env e
  return s

constraintsExp :: Env FlatPattern -> Exp FlatPattern -> Either (TypeError FlatPattern) (Vector Constraint, Subst', Type, Scheme)
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

letters :: [Text]
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

inPattern :: Type -> FlatPattern -> Infer FlatPattern a -> Infer FlatPattern a
inPattern patType pat action = case pat of
  FPName name -> do
    inEnv name (Forall [] patType) action
  FPConstructor consName names -> do
    consType <- lookupEnv consName
    let componentTypes = unArrow consType
        typeActions = (\(n, t) -> \act -> inEnv n (Forall [] t) act) <$> (zip names componentTypes)
        withTypes = foldl' (.) id $ typeActions
    withTypes action

patternTypes :: FlatPattern
             -> Infer FlatPattern [(U.Name (Exp FlatPattern), Type)]
patternTypes (FPName name) = do
  ty <- lookupEnv name
  return [(name, ty)]
patternTypes (FPConstructor _ names) = mapM (\name -> do
                                                ty <- lookupEnv name
                                                return (name, ty)) names

-- | Infer type of expression
infer :: Exp FlatPattern -> Infer FlatPattern (Type, Vector Constraint)
infer e = case e of
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])
  Lit (LPath _) -> return (typePath, [])
  Lit (LFloat _) -> return (typeFloat, [])
  Lit (LChar _ ) -> return (typeChar, [])
  Lit (LString _) -> return (typeString, [])
  Lit (LObject (Object _ consName _)) -> do
    ty <- unsafeLast . unArrow <$> lookupEnv consName
    return (ty, [])
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
  Var (V x _) -> do
    t <- lookupEnv x
    return (t, [])
  Lam b -> do
    tv <- fresh
    (pat, e') <- U.unbind b
    (t, cs) <- inPattern tv pat (infer e')
    return (tv `TArr` t, cs)
  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 <> c2 <> [Constraint (t1, t2 `TArr` tv)])
  Let binds -> do
    env <- ask
    (r, body) <- U.unbind binds
    let bindings = (\(x, U.Embed y) -> (x, y)) <$> U.unrec r
    constraints <-
      CP.foldM (\acc (pat, binding) -> do
                   tv <- fresh
                   (nAndTs, (_, c)) <- inPattern tv pat $ inEnv' (fst . fst <$> acc) $ do
                     namesAndTypes <- patternTypes pat
                     typeAndConstraints <- infer binding
                     return (namesAndTypes, typeAndConstraints)
                   case runSolve c of
                     Left err -> throwError err
                     Right sub -> do
                       let nameAndSchemes = (\(name, ty) -> ((((name, generalize (apply sub env) (apply sub ty)), c), sub))) <$> nAndTs
                       return $ acc <> nameAndSchemes) [] bindings
    let subs = snd <$> constraints
        cs = snd . fst <$> constraints
        types = fst . fst <$> constraints
    (t, c) <- inEnv' types $ local (apply (foldl' compose mempty subs)) (infer body)
    return (t, concat cs <> c)
  If cond tru fals -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tru
    (t3, c3) <- infer fals
    return (t2, c1 <> c2 <> c3 <> [Constraint (t1, typeBool), Constraint (t2, t3)])

inferTop :: Env FlatPattern -> Vector (U.Name (Exp FlatPattern), (Exp FlatPattern)) -> Either (TypeError FlatPattern) (Env FlatPattern)
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

unifies :: Type -> Type -> Solve p Subst'
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve p Subst'
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch (fromList t1) (fromList t2)

solver :: Unifier -> Solve p Subst'
solver (Unifier (su, cs)) = do
  case uncons cs of
    Nothing -> return su
    Just (Constraint (t1, t2), rest) -> do
      su1 <- unifies t1 t2
      solver $ Unifier (su1 `compose` su, apply su1 rest)

runSolve :: Vector Constraint -> Either (TypeError p) Subst'
runSolve constraints = runExcept $ unSolve $ solver $ Unifier (mempty, constraints)

bind :: TVar -> Type -> Solve p Subst'
bind a t | t == TVar a = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ Subst' $ singletonMap a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `member` ftv t

compose :: Subst' -> Subst' -> Subst'
(Subst' s1) `compose` (Subst' s2) = Subst' $ fmap (apply (Subst' s1)) s2 `union` s1

checkTush :: Env FlatPattern
          -> Text
          -> Either (ParseErrorBundle Text Void)
                    (Either (ParseErrorBundle TushTokenStream Void)
                            (Either (TypeError FlatPattern)
                                    Scheme))
checkTush env text_ = fmap (inferExp env . U.runFreshM . flattenPatterns) <$> (parseTush expP text_)

testCheckTush :: Env FlatPattern -> Text -> IO ()
testCheckTush env text_ = case checkTush env text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right x -> case x of
    Left e' -> putStr $ pack $ errorBundlePretty e'
    Right y -> case y of
      Left e'' -> print e''
      Right z -> putStrLn $ prettyPrintScheme z
