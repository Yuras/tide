{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TokenBuffer
( TokenBuffer
, Chunk (..)
, Size (..)
, Token (..)
, toList
, fromList
, measure
, null
, empty
, append
, singleton
, fromChunk
, fromChunks
, toChunks
, viewChunkLeft
, viewChunkRight
, viewLeft
, viewRight
, splitBefore
, splitAfter
, minChunkLength
, maxChunkLength
)
where

import Prelude hiding (null)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FingerTree

newtype TokenBuffer t = TokenBuffer
  { toFingerTree :: FingerTree Size (Chunk t)
  }

instance (Show t, Token t) => Show (TokenBuffer t) where
  show = show . toList

instance Token t => Eq (TokenBuffer t) where
  buf1 == buf2 = toList buf1 == toList buf2

instance Token t => Monoid (TokenBuffer t) where
  mempty = empty
  mappend = append

class Eq t => Token t where
  consumedLength :: t -> Int
  lexerLookAhead :: t -> Int

data Chunk t
  = Parsed (Vector t)
  | Dirty Int
  deriving (Eq, Show)

data Size = Size
  { chars :: !Int
  , lookAhead :: !Int
  , dirty :: !Bool
  }
  deriving (Eq, Show)

instance Monoid Size where
  mempty = Size 0 0 False
  Size chars1 lookAhead1 dirty1 `mappend` Size chars2 lookAhead2 dirty2
    = Size {..}
    where
    chars = chars1 + chars2
    lookAhead = max lookAhead2 (lookAhead1 - chars2)
    dirty = dirty1 || dirty2

newtype T t = T t

instance Token t => FingerTree.Measured Size (T t) where
  measure (T t) = Size {..}
    where
    chars = consumedLength t
    lookAhead = lexerLookAhead t
    dirty = False

instance Token t => FingerTree.Measured Size (Chunk t) where
  measure (Parsed tokens) = Vector.foldl' combine mempty tokens
    where
    combine sz t = sz `mappend` FingerTree.measure (T t)

  measure (Dirty len) = Size {..}
    where
    chars = len
    lookAhead = 0
    dirty = True

toList :: Token t => TokenBuffer t -> [Either Int t]
toList = concatMap toL . toChunks
  where
  toL (Parsed tokens) = map Right . Vector.toList $ tokens
  toL (Dirty l) = [Left l]

fromList :: Token t => [Either Int t] -> TokenBuffer t
fromList = List.foldl' step empty
  where
  step buf i = buf `append` singleton i

measure :: Token t => TokenBuffer t -> Size
measure = FingerTree.measure . toFingerTree

null :: Token t => TokenBuffer t -> Bool
null = (== 0) . chars . measure

empty :: Token t => TokenBuffer t
empty = TokenBuffer FingerTree.empty

append :: Token t => TokenBuffer t -> TokenBuffer t -> TokenBuffer t
append (TokenBuffer ft1) (TokenBuffer ft2) =
  case (FingerTree.viewr ft1, FingerTree.viewl ft2) of
    -- combine dirty chunks
    (ftl FingerTree.:> Dirty l1, Dirty l2 FingerTree.:< ftr) ->
      let chunk = Dirty (l1 + l2)
      in TokenBuffer (mconcat [ftl, FingerTree.singleton chunk, ftr])

    -- combine parsed chunks when too small
    (ftl FingerTree.:> Parsed tokens1, Parsed tokens2 FingerTree.:< ftr)
      | Vector.length tokens1 + Vector.length tokens2 < minChunkLength
      -> let chunk = Parsed (tokens1 Vector.++ tokens2)
         in TokenBuffer (mconcat [ftl, FingerTree.singleton chunk, ftr])

    _ -> TokenBuffer (ft1 FingerTree.>< ft2)

singleton :: Token t => Either Int t -> TokenBuffer t
singleton (Right t) = TokenBuffer . FingerTree.singleton $
  Parsed (Vector.singleton t)
singleton (Left l) | l <= 0 = empty
singleton (Left l) = TokenBuffer . FingerTree.singleton $ Dirty l

fromChunk :: Token t => Chunk t -> TokenBuffer t
fromChunk (Parsed tokens)
  | Vector.null tokens
  =  empty
  | Vector.length tokens > maxChunkLength
  = let (l, r) = Vector.splitAt (Vector.length tokens `div` 2) tokens
    in fromChunks [Parsed l, Parsed r]
fromChunk (Dirty len) | len <= 0 = empty
fromChunk c = TokenBuffer (FingerTree.singleton c)

fromChunks :: Token t => [Chunk t] -> TokenBuffer t
fromChunks = List.foldl' step empty
  where
  step buf c = buf `append` fromChunk c

toChunks :: TokenBuffer t -> [Chunk t]
toChunks = Foldable.toList . toFingerTree

viewChunkLeft :: Token t => TokenBuffer t -> Maybe (Chunk t, TokenBuffer t)
viewChunkLeft (TokenBuffer ft) =
  case FingerTree.viewl ft of
    FingerTree.EmptyL -> Nothing
    c FingerTree.:< rest -> Just (c, TokenBuffer rest)

viewChunkRight :: Token t => TokenBuffer t -> Maybe (TokenBuffer t, Chunk t)
viewChunkRight (TokenBuffer ft) =
  case FingerTree.viewr ft of
    FingerTree.EmptyR -> Nothing
    rest FingerTree.:> c -> Just (TokenBuffer rest, c)

viewLeft :: Token t => TokenBuffer t -> Maybe (Either Int t, TokenBuffer t)
viewLeft buf = do
  (chunk, rest) <- viewChunkLeft buf
  case chunk of
    Dirty l -> return (Left l, rest)
    Parsed tokens -> do
      let t = Vector.head tokens
          ts = Vector.drop 1 tokens
          chunk' = Parsed ts
      return (Right t, fromChunk chunk' `append` rest)

viewRight :: Token t => TokenBuffer t -> Maybe (TokenBuffer t, Either Int t)
viewRight buf = do
  (rest, chunk) <- viewChunkRight buf
  case chunk of
    Dirty l -> return (rest, Left l)
    Parsed tokens -> do
      let t = Vector.last tokens
          ts = Vector.take (pred (Vector.length tokens)) tokens
          chunk' = Parsed ts
      return (rest `append` fromChunk chunk', Right t)

-- | Split before the predicate becomes true
--
-- The second buffer is empty if the predicate never becomes true
splitBefore
  :: Token t => (Size -> Bool)
  -> TokenBuffer t -> (TokenBuffer t, TokenBuffer t)
splitBefore p (TokenBuffer tree) = go (TokenBuffer ltree) (TokenBuffer rest)
  where
  (ltree, rest) = FingerTree.split p tree
  go l r = case viewLeft r of
    Nothing -> (l, r)
    Just (t, r') ->
      let l' = l `append` singleton t
      in if p (measure l')
        then (l, r)
        else go l' r'

-- | Split after the predicate becomes true
--
-- The second buffer is empty if the predicate never becomes true
splitAfter
  :: Token t => (Size -> Bool)
  -> TokenBuffer t -> (TokenBuffer t, TokenBuffer t)
splitAfter p buf =
  case viewLeft r of
    Nothing -> (l, r)
    Just (t, r') ->
      let l' = l `append` singleton t
      in (l', r')
  where
  (l, r) = splitBefore p buf

minChunkLength :: Int
minChunkLength = 100

maxChunkLength :: Int
maxChunkLength = 1024
