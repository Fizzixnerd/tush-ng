{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Tush.Types where

import ClassyPrelude as CP hiding (TVar)

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
           | BoolT Bool
           | SymbolT Symbol
           | StringT Text
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
  chunkToTokens _ = toList
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TushTokenStream ts) = fmap TushTokenStream <$> (uncons ts)
  takeN_ n its | n <= 0 = Just (empty, its)
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
            maybe (empty, stream) CP.id mNewStreamTuple
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


data Symbol = InfixS Text
            | RegularS Text
  deriving (Eq, Ord, Show)

newtype Path = Path { unPath :: (Vector Text, PathType, FileType)}
  deriving (Eq, Ord, Show)

data PathType = PAbs | PRel | PExec | PHome
  deriving (Eq, Ord, Show)

data FileType = FTRegular | FTDirectory
  deriving (Eq, Ord, Show)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show, IsString)

data Fixity = Prefix | Infix
  deriving (Eq, Ord, Show)

data V = V Name Fixity
  deriving (Eq, Ord, Show)

data Exp
  = Var V
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
  | LFloat Double
  | LPath Path
  | LString Text
  | LChar Char
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
  = Infer { unInfer :: ExceptT TypeError (ReaderT Env (StateT InferState Identity)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadState InferState
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

instance Semigroup Env where
  (Env e1) <> (Env e2) = Env $ union e1 e2

instance Monoid Env where
  mempty = Env mempty

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

newtype Solve a = Solver { unSolve :: Except TypeError a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError TypeError
           )
