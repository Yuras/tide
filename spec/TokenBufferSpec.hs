{-# LANGUAGE RecordWildCards #-}

module TokenBufferSpec
( spec
)
where

import TokenBuffer

import Prelude hiding (null)
import Data.Maybe
import qualified Data.Vector as Vector
import Control.Monad
import Test.Hspec
import Test.QuickCheck

newtype T = T (Int, Int)
  deriving (Eq, Show)

instance Arbitrary T where
  arbitrary = do
    NonNegative a <- arbitrary
    NonNegative b <- arbitrary
    return (T (a, b))

instance Token T where
  consumedLength (T t) = fst t
  lexerLookAhead (T t) = snd t

newtype C t = C {fromC :: Chunk t}
  deriving (Show)

instance Arbitrary t => Arbitrary (C t) where
  arbitrary = do
    c <- arbitrary
    case c of
      Left (Positive len) -> do
        return (C (Dirty len))
      Right (NonEmpty ts) -> return (C (Parsed (Vector.fromList ts)))

newtype C' t = C'{fromC' :: Chunk t}
  deriving (Show)

instance Arbitrary t => Arbitrary (C' t) where
  arbitrary = do
    c <- arbitrary
    case c of
      Left len -> do
        return (C' (Dirty len))
      Right ts -> return (C' (Parsed (Vector.fromList ts)))

newtype B t = B (TokenBuffer t)
  deriving (Show)

instance (Token t, Arbitrary t) => Arbitrary (B t) where
  arbitrary = do
    chunks <- map fromC <$> arbitrary
    return (B (fromChunks chunks))

newtype NonEmptyB t = NonEmptyB (TokenBuffer t)
  deriving (Show)

instance (Token t, Arbitrary t) => Arbitrary (NonEmptyB t) where
  arbitrary = do
    NonEmpty chunks' <- arbitrary
    let chunks = map fromC chunks'
    return (NonEmptyB (fromChunks chunks))

newtype S = S Size
  deriving (Show)

instance Arbitrary S where
  arbitrary = do
    NonNegative chars <- arbitrary
    NonNegative lookAhead <- arbitrary
    dirty <- arbitrary
    return (S Size{..})

invariant :: Token t => TokenBuffer t -> Bool
invariant buf = noSuccessiveDirty
             && noSuccessiveSmallParsed
             && noLargeParsed
             && noEmptyChunks
  where
  noSuccessiveDirty = go False buf
    where
    go lastDirty b = case viewChunkLeft b of
      Nothing -> True
      Just (Parsed{}, rest) -> go False rest
      Just (Dirty{}, rest)
        | lastDirty -> False
        | otherwise -> go True rest

  noSuccessiveSmallParsed = go Nothing buf
    where
    go maybe_len b = case viewChunkLeft b of
      Nothing -> True
      Just (Dirty{}, rest) -> go Nothing rest
      Just (Parsed tokens, rest) ->
        case maybe_len of
          Nothing -> go (Just (Vector.length tokens)) rest
          Just len
            | len + Vector.length tokens < minChunkLength -> False
            | otherwise -> go (Just (Vector.length tokens)) rest

  noLargeParsed = go buf
    where
    go b = case viewChunkLeft b of
      Nothing -> True
      Just (Dirty{}, rest) -> go rest
      Just (Parsed tokens, _)
        | Vector.length tokens > maxChunkLength -> False
      Just (Parsed _, rest) -> go rest

  noEmptyChunks = all good . toChunks $ buf
    where
    good (Dirty len) | len <= 0 = False
    good (Parsed tokens) = not (Vector.null tokens)
    good _ = True

invariantT :: TokenBuffer T -> Bool
invariantT = invariant

spec :: Spec
spec = describe "TokenBuffer" $ do
  it "should be a Monoid" $ property $ \(B buf1, B buf2, B buf3) ->
    buf1 `mappend` (buf2 `mappend` buf3)
      == (buf1 `mappend` buf2) `mappend` (buf3 :: TokenBuffer T)

  describe "empty" $ do
    it "should be null" $ do
      shouldSatisfy (empty :: TokenBuffer T) null

    it "should satisfy invatiant" $
      shouldSatisfy empty invariantT

  describe "append" $ do
    it "should append all tokens" $ property $ \(tokens1, tokens2) ->
      fromList tokens1 `append` fromList tokens2
        == fromList (tokens1 ++ tokens2 :: [Either Int T])

    it "should preserve invatiant" $ property $ \(B buf1, B buf2) ->
      shouldSatisfy (buf1 `append` buf2) invariantT

  describe "fromChunks" $ do
    it "should enforce invatiant" $ property $ \cs ->
      shouldSatisfy (fromChunks . map fromC' $ cs) invariantT

  describe "singleton" $ do
    it "should enforce invatiant" $ property $ \i ->
      shouldSatisfy (singleton i) invariantT

  describe "viewChunkLeft" $ do
    it "should return Nothing on empty buffer" $
      shouldSatisfy (viewChunkLeft (empty :: TokenBuffer T)) isNothing

    it "should return the first chunk" $ do
      let c1 = Parsed (Vector.fromList [T (1, 2)])
          c2 = Dirty 3
          buf = fromChunks [c1, c2]
      viewChunkLeft buf `shouldBe` Just (c1, fromChunks [c2])

  describe "viewChunkRight" $ do
    it "should return Nothing on empty buffer" $
      shouldSatisfy (viewChunkRight (empty :: TokenBuffer T)) isNothing

    it "should return the last chunk" $ do
      let c1 = Parsed (Vector.fromList [T (1, 2)])
          c2 = Dirty 3
          buf = fromChunks [c1, c2]
      viewChunkRight buf `shouldBe` Just (fromChunks [c1], c2)

  describe "viewLeft" $ do
    it "should return Nothing on empty buffer" $
      shouldSatisfy (viewLeft (empty :: TokenBuffer T)) isNothing

    it "should return the first token" $ property $ \(t, B rest) ->
      let buf = singleton (Right t :: Either Int T) `append` rest
      in viewLeft buf `shouldBe` Just (Right t, rest)

    it "should return the first token" $ property $ \(NonEmptyB buf) ->
      let tokens = toList buf
          t = head tokens :: Either Int T
          rest = tail tokens
      in viewLeft buf `shouldBe` Just (t, fromList rest)

  describe "viewRight" $ do
    it "should return Nothing on empty buffer" $
      shouldSatisfy (viewRight (empty :: TokenBuffer T)) isNothing

    it "should return the last token" $ property $ \(NonEmptyB buf) ->
      let tokens = toList buf
          t = last tokens :: Either Int T
          rest = init tokens
      in viewRight buf `shouldBe` Just (fromList rest, t)

  describe "splitBefore" $ do
    it "should split buffer" $ property $ \(B buf) ->
      let (l, r) = splitBefore ((> 5) . chars) buf
      in (l `append` r) == (buf :: TokenBuffer T)

    it "should split buffer immediately befor the predicate becomes True"
      $ property $ \(NonEmptyB buf) ->
      let (l, r) = splitBefore p buf
          p = (> 5) . chars
      in case viewLeft (r :: TokenBuffer T) of
          Nothing ->
            shouldSatisfy l (not . p . measure)
          Just (t, _) -> do
            shouldSatisfy l (not . p . measure)
            shouldSatisfy (l `append` singleton t) (p . measure)

    it "should preserve invariant" $ property $ \(B buf) ->
      shouldSatisfy (splitBefore ((>5) . chars) buf) $ \(l, r) ->
        invariantT l && invariantT r

  describe "splitAfter" $ do
    it "should split buffer" $ property $ \(B buf) ->
      let (l, r) = splitAfter ((> 5) . chars) buf
      in (l `append` r) == (buf :: TokenBuffer T)

    it "should split buffer immediately after the predicate becomes True"
      $ property $ \(NonEmptyB buf) -> do
      let (l, r) = splitAfter p buf
          p = (> 5) . chars
      when (not (null r)) $ do
        case viewRight (l :: TokenBuffer T) of
          Nothing -> return ()
          Just (l', _) -> do
            shouldSatisfy l (p . measure)
            shouldSatisfy l' (not . p . measure)

    it "should preserve invariant" $ property $ \(B buf) ->
      shouldSatisfy (splitAfter ((>5) . chars) buf) $ \(l, r) ->
        invariantT l && invariantT r

  describe "Size" $ do
    it "should be a Monoid" $ property $ \(S s1, S s2, S s3) ->
      s1 `mappend` (s2 `mappend` s3)
        == (s1 `mappend` s2) `mappend` s3
