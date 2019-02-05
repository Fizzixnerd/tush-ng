{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Tush.Parse where

import Language.Tush.Types as T
import Language.Tush.Lex (dTokensP)

import ClassyPrelude hiding (many, some, try)
import Text.Megaparsec as MP hiding (satisfy)
import Data.Void

import Control.Monad.Fail

data TushReadState = TushReadState
newtype TushParser a
  = TushParser { unTushParser :: ParsecT Void TushTokenStream (ReaderT TushReadState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader TushReadState
           , MonadPlus
           , MonadParsec Void TushTokenStream
           , MonadFail
           , Alternative
           )

manyV :: TushParser a -> TushParser (Vector a)
manyV x = fromList <$> many x

someV :: TushParser a -> TushParser (Vector a)
someV x = fromList <$> some x

sepEndByV :: TushParser a -> TushParser sep -> TushParser (Vector a)
sepEndByV x sep = fromList <$> sepEndBy x sep

sepEndBy1V :: TushParser a -> TushParser sep -> TushParser (Vector a)
sepEndBy1V x sep = fromList <$> sepEndBy1 x sep

sepByV :: TushParser a -> TushParser sep -> TushParser (Vector a)
sepByV x sep = fromList <$> sepBy x sep

sepBy1V :: TushParser a -> TushParser sep -> TushParser (Vector a)
sepBy1V x sep = fromList <$> sepBy1 x sep

manyTween :: TushParser l -> TushParser r -> TushParser a -> TushParser (Vector a)
manyTween l r x = do
  void l
  xs <- manyV $ do
    notFollowedBy r
    x
  void r
  return xs

someTween :: TushParser l -> TushParser r -> TushParser a -> TushParser (Vector a)
someTween l r x = do
  void l
  xs <- someV $ do
    notFollowedBy r
    x
  void r
  return xs

someTill :: TushParser ender -> TushParser a -> TushParser (Vector a)
someTill e x = do
  xs <- someV $ do
    notFollowedBy e
    x
  void e
  return xs

someTill' :: TushParser ender -> TushParser a -> TushParser (Vector a, ender)
someTill' e x = do
  xs <- someV $ do
    notFollowedBy e
    x
  e_ <- e
  return (xs, e_)

manyTill :: TushParser ender -> TushParser a -> TushParser (Vector a)
manyTill e x = do
  xs <- manyV $ do
    notFollowedBy e
    x
  void e
  return xs

satisfy :: (MonadParsec e s m, MP.Token s ~ DToken) => (T.Token -> Bool) -> m T.Token
satisfy f = MP.token test mempty
  where
    test DebugToken { _dtToken = x } = if f x
                                       then Just x
                                       else Nothing

tokenP :: T.Token -> TushParser T.Token
tokenP t = satisfy (== t) MP.<?> (show t)

anyTokenP :: TushParser T.Token
anyTokenP = satisfy (const True) MP.<?> "Any Token"

symbolToName :: Symbol -> Name
symbolToName (InfixS x) = Name x
symbolToName (RegularS x) = Name x

symbolP :: TushParser Symbol
symbolP = do
  SymbolT s <- satisfy (\case
                           SymbolT _ -> True
                           _ -> False)
  return s

vP :: TushParser V
vP = do
  s <- symbolP
  case s of
    InfixS name -> return $ V (Name name) Infix
    RegularS name -> return $ V (Name name) Prefix

nameP :: TushParser Name
nameP = do
  V n _ <- vP
  return n

varP :: TushParser Exp
varP = Var <$> vP

chainlApp :: TushParser Exp -> TushParser Exp
chainlApp p = do
  a <- p
  rest a
  where
    rest a = (do
                 b <- p
                 case b of
                   Var (V _ Infix) -> do
                     c <- chainlApp p
                     rest (App (App b a) c)
                   _ -> rest (App a b))
             <|> return a

appP :: TushParser Exp
appP = do
  chainlApp $ (varP <|> lamP <|> litP <|> parensExpP)
  -- e2 <- expP
  -- case e2 of
  --   Var (V _ Infix) -> return $ App e2 e1
  --   _ -> return $ App e1 e2

lamP :: TushParser Exp
lamP = do
  void $ tokenP BSlashT
  n <- nameP
  void $ tokenP RArrowT
  e <- expP
  return $ Lam n e

letP :: TushParser Exp
letP = do
  void $ tokenP LetT
  n <- nameP
  void $ tokenP EqualsT
  e1 <- expP
  void $ tokenP InT
  e2 <- expP
  return $ Let n e1 e2

intP :: TushParser Integer
intP = do
  IntegralT i <- satisfy (\case
                             IntegralT _ -> True
                             _ -> False)
  return i

floatP :: TushParser Double
floatP = do
  FloatingT f <- satisfy (\case
                             FloatingT _ -> True
                             _ -> False)
  return f

pathP :: TushParser Path
pathP = do
  PathT p <- satisfy (\case
                         PathT _ -> True
                         _ -> False)
  return p

stringP :: TushParser Text
stringP = do
  StringT s <- satisfy (\case
                           StringT _ -> True
                           _ -> False)
  return s

charP :: TushParser Char
charP = do
  CharT c <- satisfy (\case
                         CharT _ -> True
                         _ -> False)
  return c

boolP :: TushParser Bool
boolP = do
  BoolT b <- satisfy (\case
                       BoolT _ -> True
                       _ -> False)
  return b

litP :: TushParser Exp
litP = Lit <$> (LInt <$> intP <|>
                LFloat <$> floatP <|>
                LPath <$> pathP <|>
                LString <$> stringP <|>
                LChar <$> charP <|>
                LBool <$> boolP)

ifP :: TushParser Exp
ifP = do
  void $ tokenP IfT
  cond <- expP
  void $ tokenP ThenT
  tru <- expP
  void $ tokenP ElseT
  fals <- expP
  return $ If cond tru fals

fixP :: TushParser Exp
fixP = do
  void $ tokenP FixT
  Fix <$> expP

parensExpP :: TushParser Exp
parensExpP = do
  void $ tokenP LParenT
  e <- expP
  void $ tokenP RParenT
  return e

expNoAppP :: TushParser Exp
expNoAppP = parensExpP <|>
            varP <|>
            lamP <|>
            letP <|>
            litP <|>
            ifP <|>
            fixP

expP :: TushParser Exp
expP = try appP <|> expNoAppP

parseTush :: TushParser p
          -> Text
          -> Either (MP.ParseErrorBundle Text Void)
                    (Either (MP.ParseErrorBundle TushTokenStream Void)
                            p)
parseTush p text_ = case MP.parse dTokensP "<tush>" text_ of
  Left e -> Left e
  Right x -> return $ runIdentity $ runReaderT (MP.runParserT (unTushParser p) "<tush tokens>" x) (TushReadState)

testParseTush :: Show p => TushParser p -> Text -> IO ()
testParseTush p text_ = case parseTush p text_ of
  Left e -> putStr $ pack $ errorBundlePretty e
  Right x -> case x of
    Left e -> putStr $ pack $ errorBundlePretty e
    Right x -> putStrLn $ tshow x