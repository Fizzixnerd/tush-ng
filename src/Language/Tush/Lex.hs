{-# LANGUAGE NoImplicitPrelude#-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Lex where

import Language.Tush.Types

import ClassyPrelude hiding (some, many, try)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as MPL
import Data.Void

unreservedPunctuation :: Vector (Text, Punctuation)
unreservedPunctuation = fromList [ ("=", Equals)
                                 , ("|", VBar)
                                 , ("+", Plus)
                                 , ("-", Dash)
                                 , ("*", Asterisk)
                                 , ("/", Slash)
                                 , ("!", Bang)
                                 , ("?", Interro)
                                 , (".", FullStop)
                                 , ("<", LAngle)
                                 , (">", RAngle)
                                 , (":", Colon)
                                 , ("&", Ampersand)
                                 , ("^", Caret)
                                 , ("$", Dollar)
                                 , ("@", AtSign)
                                 , ("~", Tilde)
                                 ]

reservedWords :: Vector (Text, Token)
reservedWords = fromList [ ("=", EqualsT)
                         , ("=>", FatRArrowT)
                         , ("<-", LArrowT)
                         , ("->", RArrowT)
                         , ("(", LParenT)
                         , (")", RParenT)
                         , ("{", LBraceT)
                         , ("}", RBraceT)
                         , ("[", LBracketT)
                         , ("]", RBracketT)
                         , (";", SemicolonT)
                         , ("\\", BSlashT)
                         , ("\n", NewlineT)
                         , ("let", LetT)
                         , ("in", InT)
                         , ("if", IfT)
                         , ("then", ThenT)
                         , ("else", ElseT)
                         , ("fix", FixT)
                         , ("True", BoolT True)
                         , ("False", BoolT False)
                         , ("builtin", BuiltinT)
                         ]

reservedPunctuationP :: Parsec Void Text Token
reservedPunctuationP = foldl' (<|>) empty $ (\(s, t) -> do
                                                void $ string s
                                                return t) <$> reservedWords

regularP :: Parsec Void Text Symbol
regularP = RegularS <$> regularP'

regularP' :: Parsec Void Text String
regularP' = do
  firstChar <- letterChar
  rest <- many alphaNumChar
  return $ firstChar : rest

infixP :: Parsec Void Text Symbol
infixP = (InfixS . unpack . concat <$> (some $ foldl' (<|>) empty $ string . fst <$> unreservedPunctuation))
         <|> (do
                 void $ char '`'
                 name <- regularP'
                 void $ char '`'
                 return $ InfixBackticksS name)

symbolP :: Parsec Void Text Symbol
symbolP = regularP <|> infixP

backslashN :: Parsec Void Text Char
backslashN = try $ do
  void $ char '\\'
  void $ char 'n'
  return '\n'

escapedChar :: Parsec Void Text Char
escapedChar = do
  void $ char '\\'
  anySingle

unescapedChar :: Parsec Void Text Char
unescapedChar = noneOf ['"', '\n']

stringP :: Parsec Void Text String
stringP = do
  void $ char '"'
  s <- many $ backslashN <|> escapedChar <|> unescapedChar
  void $ char '"'
  return s

commentP :: Parsec Void Text Text
commentP = do
  void $ char '#'
  c <- many $ do
    notFollowedBy eol
    anySingle
  void $ eol
  return $ pack c

sepChar :: Parsec Void Text Char
sepChar = char '/'

nonSpaceNonSepPrintChar :: Parsec Void Text Char
nonSpaceNonSepPrintChar = do
  notFollowedBy $ eitherP spaceChar sepChar
  printChar

pathBodyP :: Parsec Void Text ([String], FileType)
pathBodyP = do
  pieces <- some $ do
    void sepChar
    many nonSpaceNonSepPrintChar
  if null $ unsafeLast pieces
    then return (unsafeInit pieces, FTDirectory)
    else return (pieces, FTRegular)

pathAbsP :: Parsec Void Text Path
pathAbsP = do
  (bdy, ft) <- pathBodyP
  return $ Path (bdy, PAbs, ft)

specialPathP :: Parsec Void Text a -> PathType -> Parsec Void Text Path
specialPathP starter pt = do
  try $ void starter
  (bdy, ft) <- pathBodyP
  return $ Path (bdy, pt, ft)

pathRelP :: Parsec Void Text Path
pathRelP = specialPathP (char '.') PRel

pathExecP :: Parsec Void Text Path
pathExecP = specialPathP (char '!') PExec

pathHomeP :: Parsec Void Text Path
pathHomeP = specialPathP (char '~') PHome

pathP :: Parsec Void Text Path
pathP = pathAbsP <|> pathRelP <|> pathExecP <|> pathHomeP

integralP :: Parsec Void Text Integer
integralP = MPL.signed (return ()) MPL.decimal

floatingP :: Parsec Void Text Double
floatingP = MPL.signed (return ()) MPL.float

charP :: Parsec Void Text Char
charP = do
  void $ char '\''
  c <- backslashN <|> escapedChar <|> unescapedChar
  void $ char '\''
  return c

tokenP :: Parsec Void Text Token
tokenP = do
  t <- reservedPunctuationP <|>
       (PathT <$> pathP) <|>
       (FloatingT <$> try floatingP) <|>
       (IntegralT <$> try integralP) <|>
       (SymbolT <$> symbolP) <|>
       (StringT <$> stringP) <|>
       (CommentT <$> commentP) <|>
       (CharT <$> charP)
  many $ do
    notFollowedBy eol
    spaceChar
  return t

tokensP :: Parsec Void Text (Vector Token)
tokensP = fromList <$> (many tokenP)

mkDTokenP :: MonadParsec e s m => m Token -> m (DebugToken DebugInfo)
mkDTokenP p = do
  SourcePos _ r1 c1 <- getSourcePos
  x <- p
  SourcePos _ r2 c2 <- getSourcePos
  let di = DebugInfo (unPos r1, unPos c1) (unPos r2, unPos c2)
  return $ DebugToken di x

dTokenP :: Parsec Void Text DToken
dTokenP = mkDTokenP tokenP

dTokensP :: Parsec Void Text TushTokenStream
dTokensP = TushTokenStream . fromList <$> (many dTokenP)
