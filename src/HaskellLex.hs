{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HaskellLex
( Token (..)
, Lit (..)
, Special (..)
, ReservedOp (..)
, ReservedId (..)
, White (..)
, State (..)
, initialState
, next
, tokens
)
where

import Prelude hiding (takeWhile)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Control.Monad
import Control.Applicative

data Token
  = VarId [Text] Text
  | ConId [Text] Text
  | VarSym [Text] Text
  | ConSym [Text] Text
  | Lit Lit
  | Special Special
  | ReservedOp ReservedOp
  | ReservedId ReservedId
  | White White
  | EOF
  | Error Text
  deriving (Show, Eq)

data Lit
  = Integer Integer
  | Float Double
  | Char Char
  | String Text
  deriving (Show, Eq)

data Special
  = LeftParen
  | RighParen
  | Comma
  | Semicolon
  | LeftSquare
  | RightSquare
  | BackQuote
  | LeftCurly
  | RightCurly
  deriving (Show, Eq)

data ReservedOp
  = DotDot
  | Colon
  | DoubleColon
  | Equal
  | Backslash
  | Bar
  | LeftArrow
  | RightArrow
  | At
  | Tilde
  | DoubleArrow
  deriving (Show, Eq)

data ReservedId
  = Case
  | Class
  | Data
  | Default
  | Deriving
  | Do
  | Else
  | Foreign
  | If
  | Import
  | In
  | Infix
  | Infixl
  | Infixr
  | Instance
  | Let
  | Module
  | Newtype
  | Of
  | Then
  | Type
  | Where
  | Underscope
  deriving (Show, Eq)

-- | White spaces
--
-- Short comments are represented as a single `Comment`.
--
-- If lexer can't find end of the comment shortly, it emits `CommentStart`,
-- then a number of `Comment` tokens and finally `CommentEnd`.
data White
  = WhiteSpace Text
  | Comment Text
  | CommentStart Text
  | CommentEnd Text
  deriving (Show, Eq)

data State
  = InDefault
  | InComment
  deriving (Show, Eq)

initialState :: State
initialState = InDefault

tokens :: Parser [Token]
tokens = go [] initialState
  where
  go res InDefault = do
    (s', t) <- choice
      [ next InDefault
      , const (InDefault, EOF) <$> endOfInput
      ]
    case t of
      EOF -> return (reverse (t:res))
      _ -> go (t:res) s'
  go res InComment = do
    (s', t) <- next InComment
    go (t:res) s'

next :: State -> Parser (State, Token)
next InComment = choice
  [ fmap White <$> white InComment
  , (InDefault,) . Error . Text.singleton <$> anyChar
  , pure (InDefault, Error "")
  ]
next s = do
  choice
    [ (s,) <$> qualifiedThing
    , (s,) . Lit <$> literal
    , fmap White <$> white s
    , (s,) . Special <$> special
    , (s,) . ReservedOp <$> reservedOp
    , (s,) . ReservedId <$> reservedId
    , (InDefault,) . Error . Text.singleton <$> anyChar
    , const (InDefault, EOF) <$> endOfInput
    ]

white :: State -> Parser (State, White)
white InComment =
  multiComment InComment
white s = choice
  [ (s,) . WhiteSpace <$> takeWhile1 Char.isSpace
  , (s,) . Comment <$> lineComment
  , multiComment s
  ]

multiComment :: State -> Parser (State, White)
multiComment InDefault = do
  str <- string "{-"
  return (InComment, CommentStart str)

multiComment InComment = do
  str <- Text.pack <$> manyTill_ anyChar (string "-}" <|> string "\n")
  if "-}" `Text.isSuffixOf` str
    then return (InDefault, CommentEnd str)
    else return (InComment, Comment str)

lineComment :: Parser Text
lineComment = do
  dashes <- takeWhile1 (== '-')
  unless (Text.length dashes >= 2)
    mzero
  rest <- takeWhile (/= '\n')
  return $ Text.append dashes rest

literal :: Parser Lit
literal = do
  choice
    [ numberLit
    , stringOrCharLit
    ]

stringOrCharLit :: Parser Lit
stringOrCharLit = do
  c <- anyChar
  case c of
    '\'' -> Char <$> charLit
    '"' -> String <$> stringLit
    _ -> mzero

charLit :: Parser Char
charLit = do
  c <- anyChar
  void $ char '\''
  return c

stringLit :: Parser Text
stringLit = do
  str <- Text.pack <$> manyTill_ anyChar (string "\"" <|> string "\n")
  if "\"" `Text.isSuffixOf` str
    then return (Text.init str)
    else mzero

manyTill_ :: Parser Char -> Parser Text -> Parser String
manyTill_ p end = scanner
  where
  scanner = fmap Text.unpack end <|> liftA2 (:) p scanner

numberLit :: Parser Lit
numberLit = do
  pref <- takeWhile1 isDigit
  choice
    [ floatLit pref
    , integralLit pref
    ]
  where
  floatLit pref = do
    void $ char '.'
    rest <- takeWhile isDigit
    return . Float $ read $ Text.unpack (Text.concat [pref, ".", rest])

  integralLit pref = pure . Integer $ read (Text.unpack pref)

qualifiedThing :: Parser Token
qualifiedThing = do
  cons <- conIds <|> pure []
  choice
    [ qualThing cons
    , qconId cons
    ]
  where
  qconId cons | null cons = mzero
  qconId [c] = pure (ConId [] c)
  qconId cons = pure (ConId (init cons) (last cons))

  qualThing cons = do
    unless (null cons) $
      void $ char '.'
    choice
      [ qvarId cons
      , qvarSym cons
      , qconSym cons
      ]

  qvarId cons = do
    var <- varId
    return (VarId cons var)

  qvarSym cons = do
    sym <- varSym
    return (VarSym cons sym)

  qconSym cons = do
    sym <- conSym
    return (ConSym cons sym)

varSym :: Parser Text
varSym = do
  res <- scan True scanner
  when (Text.null res || isReservedOp res || isDashes res)
    mzero
  return res
  where
  scanner True ':' = Nothing
  scanner _ ch 
    | isSymbol ch
    = Just False
  scanner _ _ = Nothing

conSym :: Parser Text
conSym = do
  res <- scan True scanner
  when (Text.null res || isReservedOp res || isDashes res)
    mzero
  return res
  where
  scanner True ':' = Just False
  scanner True _ = Nothing
  scanner _ ch 
    | isSymbol ch
    = Just False
  scanner _ _ = Nothing

varId :: Parser Text
varId = do
  res <- scan True scanner
  when (Text.null res || isReservedId res)
    mzero
  return res
  where
  scanner True ch
    | isSmall ch
    = Just False
  scanner False ch
    | isVarChar ch
    = Just False
  scanner _  _ = Nothing

-- A.B.C -> ["A", "B", "C"]
conIds :: Parser [Text]
conIds = do
  con <- conId
  cons <- many' $ do
    void $ char '.'
    conId
  return ([con] ++ cons)

conId :: Parser Text
conId = do
  res <- scan True scanner
  when (Text.null res)
    mzero
  return res
  where
  -- True means the first char
  scanner True ch
    | isLarge ch
    = Just False
  scanner False ch
    | isVarChar ch
    = Just False
  scanner _ _ = Nothing

special :: Parser Special
special = choice
  [ chr '(' LeftParen
  , chr ')' RighParen
  , chr ',' Comma
  , chr ';' Semicolon
  , chr '[' LeftSquare
  , chr ']' RightSquare
  , chr '`' BackQuote
  , chr '{' LeftCurly
  , chr '}' RightCurly
  ]
  where
  chr c con = const con <$> char c

reservedOp :: Parser ReservedOp
reservedOp = choice
  [ str ".." DotDot
  , str ":"  Colon
  , str "::" DoubleColon
  , str "="  Equal
  , str "\\" Backslash
  , str "|"  Bar
  , str "<-" LeftArrow
  , str "->" RightArrow
  , str "@"  At
  , str "~"  Tilde
  , str "=>" DoubleArrow
  ]
  where
  str s con = const con <$> string s

reservedId :: Parser ReservedId
reservedId = choice
  [ str "case"     Case
  , str "class"    Class
  , str "data"     Data
  , str "default"  Default
  , str "deriving" Deriving
  , str "do"       Do
  , str "else"     Else
  , str "foreign"  Foreign
  , str "if"       If
  , str "import"   Import
  , str "instance" Instance
  , str "infixl"   Infixl
  , str "infixr"   Infixr
  , str "infix"    Infix
  , str "in"       In
  , str "let"      Let
  , str "module"   Module
  , str "newtype"  Newtype
  , str "of"       Of
  , str "then"     Then
  , str "type"     Type
  , str "where"    Where
  , str "_"        Underscope
  ]
  where
  str s con = const con <$> string s

isDashes :: Text -> Bool
isDashes txt = "--" `Text.isPrefixOf` txt && Text.all (== '-') txt

isSymbol :: Char -> Bool
isSymbol ch | isSpecial ch = False
isSymbol '_' = False
isSymbol '"' = False
isSymbol '\'' = False
isSymbol ch | Char.isSymbol ch = True
isSymbol ch | Char.isPunctuation ch = True
isSymbol _ = False

isVarChar :: Char -> Bool
isVarChar ch | isSmall ch = True
isVarChar ch | isLarge ch = True
isVarChar ch | isDigit ch = True
isVarChar '\'' = True
isVarChar _ = False

isSmall :: Char -> Bool
isSmall '_' = True
isSmall ch | Char.isUpper ch = True
isSmall ch | Char.isLower ch = True
isSmall _ = False

isLarge :: Char -> Bool
isLarge = Char.isUpper

isDigit :: Char -> Bool
isDigit = Char.isDigit

isSpecial :: Char -> Bool
isSpecial = (`elem` ("(),;[]`{}" :: String))

isReservedId :: Text -> Bool
isReservedId = (`elem` keywords)
  where
  keywords =
    [ "case", "class", "data", "default", "deriving", "do", "else"
    , "foreign", "if", "import", "in", "infix", "infixl"
    , "infixr", "instance", "let", "module", "newtype", "of"
    , "then", "type", "where", "_"
    ]

isReservedOp :: Text -> Bool
isReservedOp = (`elem` ops)
  where
  ops =
    [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"
    ]
