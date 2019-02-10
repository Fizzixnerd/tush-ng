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
import Unsafe.Coerce

import GHC.Generics (Generic)

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Hashable as H
import qualified Text.Megaparsec.Stream as Stream
import Text.Megaparsec hiding (Token, ParseError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Data.Void

data TushError = LexErr (ParseErrorBundle Text Void)
               | ParseErr (ParseErrorBundle TushTokenStream Void)
               | TypeErr (TypeError FlatPattern) (Exp FlatPattern)
  deriving (Eq, Show)

type Result a = Either TushError a

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
           | VBarT
           | NewlineT
           | LetT
           | InT
           | IfT
           | ThenT
           | ElseT
           | FixT
           | BuiltinT
           | DataT
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
data DebugInfo = DebugInfo
  { _diStart :: !(Row, Col)
  , _diEnd :: !(Row, Col)
  } deriving (Eq, Ord, Show)

-- | A `Token' with attached debug information; the parser never sees
-- the debug information directly and so doesn't need to worry about
-- it.
data DebugToken d = DebugToken
  { _dtInfo :: !d
  , _dtToken :: !Token
  } deriving (Eq, Ord, Show)

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

newtype Name' = Name' { unName' :: String }
  deriving (Eq, Ord, Show, IsString)

data Fixity' = Prefix | Infix | InfixBackticks
  deriving (Eq, Ord, Show, Generic)

instance Alpha Fixity'

instance (Alpha p, Typeable p) => Eq (Bind p (Exp p)) where
  (==) = aeq

instance (Alpha p, Typeable p) => Eq (Bind (Rec [(p, Embed (Exp p))]) (Exp p)) where
  (==) = aeq

data V a = V (Name a) Fixity'
  deriving (Eq, Show, Generic)

instance Typeable a => Alpha (V a)

-- | Expressions parameterized on the pattern type they support.
data Exp p
  = Var (V (Exp p))
  | App (Exp p) (Exp p)
  | Lam (Bind p (Exp p))
  | Let (Bind (Rec [(p, Embed (Exp p))]) (Exp p))
  | Lit (Lit p)
  | If (Exp p) (Exp p) (Exp p)
  | Builtin Builtin
  deriving (Eq, Show, Generic)

instance (Alpha p, Typeable p) => Alpha (Exp p)

data Builtin
  = IAdd
  | ISub
  | IMul
  | IDiv
  | IRem
  | IEql
  | INeq
  | BNot
  | BXor
  | ONth
  deriving (Eq, Show, Generic)

instance Alpha Builtin

data Lit p
  = LInt Integer
  | LFloat Double
  | LPath Path
  | LString String
  | LChar Char
  | LBool Bool
  | LObject (Object p)
  deriving (Eq, Show, Generic)

instance (Alpha p, Typeable p) => Alpha (Lit p)

data Object p = Object
  { oType :: Name Type
  , oTag :: Name (Exp p)
  , oContents :: [Exp p]
  } deriving (Eq, Show, Generic)

instance (Alpha p, Typeable p) => Alpha (Object p)

newtype ConstructorName = ConstructorName { unConstructorName :: String }
  deriving (Eq, Show, Generic)

instance Alpha ConstructorName

data Pattern = PName (Name (Exp Pattern))
             | PConstructor ConstructorName [Pattern]
  deriving (Eq, Show, Generic)

instance Alpha Pattern

data FlatPattern = FPName (Name (Exp FlatPattern))
                 | FPConstructor ConstructorName [Name (Exp FlatPattern)]
  deriving (Eq, Show, Generic)

instance Alpha FlatPattern

newtype PlainName = PlainName (Name (Exp PlainName))
  deriving (Eq, Show, Generic)

instance Alpha PlainName

instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) (Exp p) where
  isvar (Var (V x _)) = Just (SubstName x)
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) Pattern where
  isvar _ = Nothing
instance Subst (Exp FlatPattern) FlatPattern where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) ConstructorName where
  isvar _ = Nothing
instance Subst (Exp PlainName) PlainName where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) (V (Exp p)) where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) Fixity' where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) (Lit p) where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) Path where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) PathType where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) FileType where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) Builtin where
  isvar _ = Nothing
instance (Alpha p, Subst (Exp p) p, Typeable p) => Subst (Exp p) (Object p) where
  isvar _ = Nothing

data Program p = Program [Def p]
  deriving (Eq, Show)

data Data p = Data
  { ddName :: Name Type
  , ddProducts :: [DataProduct p]
  } deriving (Eq, Show)

data DataProduct p = DataProduct
  { dpName :: Name (Exp p)
  , dpType :: [Type]
  } deriving (Eq, Show)

data Def p = ValDef (Name (Exp p), Embed (Exp p))
           | DataDef (Data p)
  deriving (Eq, Show)

newtype TVar = TV Text
  deriving (Eq, Ord, Show, IsString)

data Type
  = TVar TVar
  | TCon Name'
  | TArr Type Type
  deriving (Eq, Ord, Show)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord, Show)

newtype Infer p a
  = Infer { unInfer :: ExceptT (TypeError p) (ReaderT (Env p) (StateT InferState FreshM)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Env p)
           , MonadState InferState
           , MonadError (TypeError p)
           , Fresh
           )

data InferState = InferState { count :: Int }
  deriving (Eq, Ord, Show)

data TypeError p
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable (Name (Exp p))
  | Ambiguous (Vector Constraint)
  | UnificationMismatch (Vector Type) (Vector Type)
  deriving (Eq, Ord, Show)

newtype Constraint = Constraint { unConstraint :: (Type, Type) }
  deriving (Eq, Ord, Show)

newtype Env p = Env { unEnv :: Map (Name (Exp p)) Scheme }
  deriving (Eq, Show)

instance Semigroup (Env p) where
  (Env e1) <> (Env e2) = Env $ CP.union e1 e2

instance Monoid (Env p) where
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

instance Substitutable (Env p) where
  apply s (Env env) = Env $ fmap (Language.Tush.Types.apply s) env
  ftv (Env env) = ftv $ Map.elems env

newtype Unifier = Unifier { unUnifier :: (Subst', Vector Constraint) }
  deriving (Eq, Ord, Show)

newtype Solve p a = Solver { unSolve :: Except (TypeError p) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError (TypeError p)
           )
