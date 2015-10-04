{-# LANGUAGE MultiParamTypeClasses #-}

module TextBuffer
( TextBuffer (..)
, Chunk (..)
, empty
, append
, null
, fromText
, toText
, fromLazyText
, toLazyText
, splitAt
, replace
, minChunkLength
, maxChunkLength
)
where

import Span

import Prelude hiding (null, splitAt, span)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Foldable as Foldable
import Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FingerTree

newtype TextBuffer = TextBuffer
  { toFingerTree :: FingerTree Size Chunk
  }

instance Show TextBuffer where
  show = show . toLazyText

instance Eq TextBuffer where
  buf1 == buf2 = toLazyText buf1 == toLazyText buf2

instance Monoid TextBuffer where
  mempty = empty
  mappend = append

newtype Chunk = Chunk {getText :: Text}

newtype Size = Size {getSize :: Int}

instance Monoid Size where
  mempty = Size 0
  Size s1 `mappend` Size s2 = Size (s1 + s2)

instance FingerTree.Measured Size Chunk where
  measure = Size . Text.length . getText

empty :: TextBuffer
empty = TextBuffer FingerTree.empty

append :: TextBuffer -> TextBuffer -> TextBuffer
append (TextBuffer tree1) (TextBuffer tree2) =
  case (FingerTree.viewr tree1, FingerTree.viewl tree2) of
    (l FingerTree.:> Chunk c1, Chunk c2 FingerTree.:< r)
      | Text.length c1 + Text.length c2 < minChunkLength
      -> let c = FingerTree.singleton . Chunk $ c1 `Text.append` c2
         in TextBuffer (mconcat [l, c, r])

    _ -> TextBuffer (tree1 FingerTree.>< tree2)

null :: TextBuffer -> Bool
null = (== 0) . getSize . FingerTree.measure . toFingerTree

fromText :: Text -> TextBuffer
fromText txt
  | Text.null txt
  = empty
  | Text.length txt > maxChunkLength
  = let (l, r) = Text.splitAt (Text.length txt `div` 2) txt
    in fromText l `append` fromText r
fromText txt = TextBuffer . FingerTree.singleton . Chunk $ txt

toText :: TextBuffer -> Text
toText = Lazy.Text.toStrict . toLazyText

fromLazyText :: Lazy.Text -> TextBuffer
fromLazyText = List.foldl' step empty . Lazy.Text.toChunks
  where
  step res txt = res `append` fromText txt

toLazyText :: TextBuffer -> Lazy.Text
toLazyText = Lazy.Text.fromChunks . map getText . Foldable.toList . toFingerTree

splitAt :: Int -> TextBuffer -> (TextBuffer, TextBuffer)
splitAt n (TextBuffer tree) =
  case FingerTree.viewl rest of
    FingerTree.EmptyL -> (TextBuffer l, TextBuffer rest)
    Chunk txt FingerTree.:< r ->
      let (c1, c2) = Text.splitAt (n - getSize (FingerTree.measure l)) txt
          l' = TextBuffer l `append` fromText c1
          r' = fromText c2 `append` TextBuffer r
      in (l', r')
  where
  (l, rest) = FingerTree.split ((> n) . getSize) tree

replace :: Span -> Text -> TextBuffer -> TextBuffer
replace span txt buf = mconcat [lbuf, mbuf, rbuf]
  where
  (lbuf, rest) = splitAt (start span) buf
  (_, rbuf) = splitAt (count span) rest
  mbuf = fromText txt

minChunkLength :: Int
minChunkLength = 100

maxChunkLength :: Int
maxChunkLength = 1024
