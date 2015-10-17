{-# LANGUAGE RecordWildCards #-}

module Lex
( Lex (..)
, Item (..)
, empty
, replace
, isDirty
, step
, tokens
)
where

import TextBuffer (TextBuffer)
import qualified TextBuffer
import TokenBuffer (TokenBuffer)
import qualified TokenBuffer
import Span

import Prelude hiding (lex, span)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto

data Lex s t = Lex
  { textBuffer :: TextBuffer
  , tokenBuffer :: TokenBuffer (T s t)
  , initialState :: s
  , tokenize :: s -> Atto.Parser (s, t)
  }

empty :: (Eq t, Eq s) => s -> (s -> Atto.Parser (s, t)) -> Lex s t
empty s tok = Lex
  { textBuffer = TextBuffer.empty
  , tokenBuffer = TokenBuffer.empty
  , initialState = s
  , tokenize = tok
  }

type T s t = Item s t

data Item s a = Item
  { token :: a
  , lexerState :: s
  , consumedLength :: Int
  , lexerLookAhead :: Int
  }
  deriving (Eq, Show)

instance (Eq t, Eq s) => TokenBuffer.Token (Item s t) where
  consumedLength = consumedLength
  lexerLookAhead = lexerLookAhead

-- | Replace the specified span with the text
--
-- Returns new lex and an affected span (before the change)
replace :: (Eq t, Eq s) => Lex s t -> Span -> Text -> (Lex s t, Span)
replace lex span txt = (lex', span')
  where
  (tokenBuffer', span') = markDirty (tokenBuffer lex) span (Text.length txt)
  lex' = lex
    { textBuffer = TextBuffer.replace span txt (textBuffer lex)
    , tokenBuffer = tokenBuffer'
    }

markDirty :: (Eq t, Eq s) => TokenBuffer (T s t) -> Span -> Int -> (TokenBuffer (T s t), Span)
markDirty buf span len = (buf', span')
  where
  (lbuf, rest) = TokenBuffer.splitBefore pBefore buf
  pBefore TokenBuffer.Size{..} = chars + lookAhead > start span
  lbufLen = TokenBuffer.chars (TokenBuffer.measure lbuf)

  (mbuf, rbuf) = TokenBuffer.splitAfter pAfter rest
  pAfter TokenBuffer.Size{..} = chars > (start span - lbufLen + count span)
  mbufLen = TokenBuffer.chars (TokenBuffer.measure mbuf)

  c = TokenBuffer.Dirty (mbufLen - count span + len)
  buf' = mconcat [lbuf, TokenBuffer.fromChunk c, rbuf]
  span' = Span lbufLen mbufLen

-- | Process dirty sections if any
--
-- Returns affected span
step :: (Eq t, Eq s) => Lex s t -> (Lex s t, Span)
step lex | not (isDirty lex) = (lex, Span 0 0)
step lex = (lex', Span startPos (endPos - startPos))
  where
  (lbuf, rest) = TokenBuffer.splitBefore TokenBuffer.dirty (tokenBuffer lex)
  startPos = TokenBuffer.chars (TokenBuffer.measure lbuf)
  lastState =
    case TokenBuffer.viewRight lbuf of
      Just (_, Right t) -> lexerState t
      _ -> initialState lex
  (_, txt) = TextBuffer.splitAt startPos (textBuffer lex)
  (mbuf, rbuf) = reparse lastState txt rest
  endPos = startPos + TokenBuffer.chars (TokenBuffer.measure mbuf)
  buf = mconcat [lbuf, mbuf, rbuf]
  lex' = lex
    { tokenBuffer = buf
    }

  reparse s txtBuf tBuf =
    let p = Atto.parse (tokenize lex s)
    in go txtBuf tBuf 0 TokenBuffer.empty p 0 (100 :: Int)

  go _ tBuf tOff res _ _ 0 =
    let (tBuf', _) = markDirty tBuf (Span 0 (tOff + 1)) 1
    in (res, tBuf')
  go txtBuf tBuf tOff res p lahead n =
    case p (TextBuffer.toText txtL) of
      Atto.Fail _ _ msg -> error $ "Lexer failed: " ++ msg
      Atto.Partial p' -> go txtR tBuf (tOff + txtLLen) res p' (lahead + txtLLen) n
      Atto.Done txtRest (s, tok) ->
        let item = Item
              { token = tok
              , lexerState = s
              , consumedLength = lahead + txtLLen - Text.length txtRest
              , lexerLookAhead = Text.length txtRest + 1
              }
            res' = res `TokenBuffer.append` TokenBuffer.singleton (Right item)
            (tBuf', tBuf'') = TokenBuffer.splitBefore
              ((> tOff + consumedLength item) . TokenBuffer.chars) tBuf
            tOff' = tOff + consumedLength item - TokenBuffer.chars (TokenBuffer.measure tBuf')
            txtBuf' = TextBuffer.fromText txtRest `TextBuffer.append` txtR
            done = tOff == 0 && done'
            done' = case TokenBuffer.viewLeft tBuf of
                      Just (Right item', _)
                        | item' == item -> True
                      _ -> False
            p' = Atto.parse (tokenize lex s)
        in if TextBuffer.null txtBuf'
            then (res', TokenBuffer.empty)
            else if done
              then (res, tBuf)
              else go txtBuf' tBuf'' tOff' res' p' 0 (pred n)
    where
    (txtL, txtR) = TextBuffer.splitAt 100 txtBuf
    txtLLen = Text.length (TextBuffer.toText txtL)

-- | Whether needs processing
isDirty :: (Eq t, Eq s) => Lex s t -> Bool
isDirty = TokenBuffer.dirty . TokenBuffer.measure . tokenBuffer

-- | Get tokens for the span
--
-- The requested span can start in the middle of a token, so we return
-- the offset of the nearest token before the span as the first element
-- of the tuple.
tokens :: (Eq t, Eq s) => Lex s t -> Span -> (Int, [Either Int (T s t)])
tokens lex span = (startPos, toChunks buf)
  where
  (lbuf, rest) = TokenBuffer.splitBefore
    ((> start span) . TokenBuffer.chars)
    (tokenBuffer lex)
  startPos = TokenBuffer.chars (TokenBuffer.measure lbuf)
  (buf, _) = TokenBuffer.splitAfter
    ((> start span - startPos + count span) . TokenBuffer.chars) rest
  toChunks = TokenBuffer.toList
