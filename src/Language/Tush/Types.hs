{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Tush.Types where

import ClassyPrelude as CP hiding (TVar)

import Unbound.Generics.LocallyNameless

import GHC.Generics (Generic)

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Text.Megaparsec.Stream as Stream
import Text.Megaparsec hiding (Token)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

-- * Syntax
data Punctuation = Equals
                 | VBar
                 | Plus
                 | Dash
                 | Asterisk
                 | Slash
                 | Bang
                 | Interro
                 | FullStop
                 | LAngle
                 | RAngle
                 | Colon
                 | Ampersand
                 | Caret
                 | Dollar
                 | AtSign
                 | Tilde
  deriving (Eq, Ord, Show)

data Token = LArrowT
           | RArrowT
           | FatRArrowT
           | EqualsT
           | LParenT
           | RParenT
           | LBraceT
           | RBraceT
           | LBracketT
           | RBracketT
           | ColonT -- single colon
           | SemicolonT
           | BSlashT
           | NewlineT
           | LetT
           | InT
           | IfT
           | ThenT
           | ElseT
           | FixT
           | BuiltinT
           | BoolT Bool
           | SymbolT Symbol
           | StringT String
           | CommentT Text
           | PathT Path
           | IntegralT Integer
           | FloatingT Double
           | CharT Char
  deriving (Eq, Ord, Show)

-- | Type synonym for keeping track of which row we are on.
type Row = Int

-- | Type synonym for keeping track of which column we are on.
type Col = Int

-- | The current debug information kept around so that we can tell the
-- user where an error occured.  More can be added later without
-- breaking much code.
data DebugInfo = DebugInfo { _diStart :: !(Row, Col)
                           , _diEnd :: !(Row, Col)
                           }
  deriving (Eq, Ord, Show)

-- | A `Token' with attached debug information; the parser never sees
-- the debug information directly and so doesn't need to worry about
-- it.
data DebugToken d = DebugToken { _dtInfo :: !d
                               , _dtToken :: !Token
                               }
  deriving (Eq, Ord, Show)

-- | Type synonym for `DebugToken' instantiated on our currently used
-- `DebugInfo'
type DToken = DebugToken DebugInfo

newtype TushTokenStream = TushTokenStream { unStream :: Vector DToken }
  deriving (Eq, Show)

-- | Megaparsec Stream instance so that this properly works with the
-- rest of the library.
instance Stream.Stream TushTokenStream where
  type Token TushTokenStream = DToken
  type Tokens TushTokenStream = Vector DToken
  tokensToChunk _ = fromList
  chunkToTokens _ = CP.toList
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TushTokenStream ts) = fmap TushTokenStream <$> (uncons ts)
  takeN_ n its | n <= 0 = Just (CP.empty, its)
  takeN_ _ (TushTokenStream ts) | null ts = Nothing
  takeN_ n (TushTokenStream ts) = Just $ TushTokenStream <$> (splitAt n ts)
  takeWhile_ p_ (TushTokenStream ts) = TushTokenStream <$> (span p_ ts)
  showTokens _ (x NE.:| xs) = show $ (\DebugToken {..} -> _dtToken) <$> (x : xs)
  reachOffset newOffset pstate =
    let oldOffset = pstateOffset pstate
        oldSourcePos = pstateSourcePos pstate
        oldStream = pstateInput pstate
        getNewStream offset stream =
          let mNewStreamTuple = takeN_ offset stream
          in
            maybe (CP.empty, stream) CP.id mNewStreamTuple
        getNewSourcePos stream old =
          let mNextToken = fst <$> take1_ stream
          in
            maybe old (\dtoken ->
                          old
                          { sourceLine = mkPos $ fst $ _diStart $ _dtInfo dtoken
                          , sourceColumn = mkPos $ snd $ _diStart $ _dtInfo dtoken
                          }) mNextToken
        (jumpedContent, newStream) = getNewStream (newOffset - oldOffset) oldStream
        newlineIndices = V.findIndices (\dtoken ->
                                         _dtToken dtoken == NewlineT) jumpedContent
        lastIndex = if null newlineIndices
                    then Nothing
                    else Just $ V.last newlineIndices
        prefix = maybe jumpedContent (\idx -> snd $ splitAt idx jumpedContent) lastIndex
        restOfLine = fst $ takeWhile_ (\dtoken -> _dtToken dtoken /= NewlineT) newStream
        wholeLine = prefix ++ restOfLine
        printedLine = if null wholeLine
                      then "<empty line>"
                      else concatMap (show . _dtToken) wholeLine
        newSourcePos = getNewSourcePos oldStream oldSourcePos
        newPosState = pstate
                      { pstateInput = newStream
                      , pstateOffset = newOffset
                      , pstateSourcePos = newSourcePos
                      }
    in
      (newSourcePos, unpack printedLine, newPosState)

data Symbol = InfixS String
            | InfixBackticksS String
            | RegularS String
  deriving (Eq, Ord, Show)

newtype Path = Path { unPath :: ([String], PathType, FileType)}
  deriving (Eq, Ord, Show, Generic)

instance Alpha Path

data PathType = PAbs | PRel | PExec | PHome
  deriving (Eq, Ord, Show, Generic)

instance Alpha PathType

data FileType = FTRegular | FTDirectory
  deriving (Eq, Ord, Show, Generic)

instance Alpha FileType

newtype Name' = Name' { unName' :: Text }
  deriving (Eq, Ord, Show, IsString)

data Fixity' = Prefix | Infix | InfixBackticks
  deriving (Eq, Ord, Show, Generic)

instance Alpha Fixity'

instance Eq (Bind (Name Exp) Exp) where
  (==) = aeq

data V = V (Name Exp) Fixity'
  deriving (Eq, Show, Generic)

instance Alpha V

data Exp
  = Var V
  | App Exp Exp
  | Lam (Bind (Name Exp) Exp)
  | Let (Bind (Name Exp) Exp) Exp
  | Lit Lit
  | If Exp Exp Exp
  | Fix Exp
  | Builtin Builtin
  deriving (Eq, Show, Generic)

instance Alpha Exp

data Builtin
  = IAdd
  | ISub
  | IMul
  | IDiv
  | IEql
  | INeq
  | BNot
  | BXor
  deriving (Eq, Show, Generic)

instance Alpha Builtin

data Lit
  = LInt Integer
  | LFloat Double
  | LPath Path
  | LString String
  | LChar Char
  | LBool Bool
  deriving (Eq, Ord, Show, Generic)

instance Alpha Lit
instance Subst Exp Exp where
  isvar (Var (V x _)) = Just (SubstName x)
  isvar _ = Nothing
instance Subst Exp V where
  isvar _ = Nothing
instance Subst Exp Fixity' where
  isvar _ = Nothing
instance Subst Exp Lit where
  isvar _ = Nothing
instance Subst Exp Path where
  isvar _ = Nothing
instance Subst Exp PathType where
  isvar _ = Nothing
instance Subst Exp FileType where
  isvar _ = Nothing
instance Subst Exp Builtin where
  isvar _ = Nothing

data Program = Program (Vector Def) Exp
  deriving (Show)

data Def = Def (Bind (Name Exp) Exp)
  deriving (Show)

newtype TVar = TV Text
  deriving (Eq, Ord, Show)

data Type
  = TVar TVar
  | TCon Name'
  | TArr Type Type
  deriving (Eq, Ord, Show)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype Infer a
  = Infer { unInfer :: ExceptT TypeError (ReaderT Env (StateT InferState FreshM)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadState InferState
           , MonadError TypeError
           , Fresh
           )

data InferState = InferState { count :: Int }
  deriving (Eq, Ord, Show)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable (Name Exp)
  | Ambiguous (Vector Constraint)
  | UnificationMismatch (Vector Type) (Vector Type)
  deriving (Eq, Ord, Show)

newtype Constraint = Constraint { unConstraint :: (Type, Type) }
  deriving (Eq, Ord, Show)

newtype Env = Env { unEnv :: Map (Name Exp) Scheme }
  deriving (Eq, Show)

instance Semigroup Env where
  (Env e1) <> (Env e2) = Env $ CP.union e1 e2

instance Monoid Env where
  mempty = Env mempty

newtype Subst' = Subst' (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst' -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply (Subst' s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = Language.Tush.Types.apply s t1 `TArr` Language.Tush.Types.apply s t2

  ftv TCon{}         = mempty
  ftv (TVar a)       = CP.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `CP.union` ftv t2

instance Substitutable Scheme where
  apply (Subst' s) (Forall as t) = Forall as $ Language.Tush.Types.apply s' t
    where s' = Subst' $ foldr Map.delete s as
  ftv (Forall as t) = ftv t \\ setFromList as

instance Substitutable Constraint where
   apply s (Constraint (t1, t2)) = Constraint (Language.Tush.Types.apply s t1, Language.Tush.Types.apply s t2)
   ftv (Constraint (t1, t2)) = ftv t1 `CP.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . Language.Tush.Types.apply
  ftv   = foldr (CP.union . ftv) mempty

instance Substitutable a => Substitutable (Vector a) where
  apply = fmap . Language.Tush.Types.apply
  ftv = foldr (CP.union . ftv) mempty

instance Substitutable Env where
  apply s (Env env) = Env $ fmap (Language.Tush.Types.apply s) env
  ftv (Env env) = ftv $ Map.elems env

newtype Unifier = Unifier { unUnifier :: (Subst', Vector Constraint) }
  deriving (Eq, Ord, Show)

newtype Solve a = Solver { unSolve :: Except TypeError a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError TypeError
           )
