{-# LANGUAGE OverloadedStrings #-}

module TextBufferSpec
( spec
)
where

import TextBuffer
import Span

import Prelude hiding (null, splitAt)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.FingerTree as FingerTree
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "TextBuffer" $ do
  it "should be Monoid" $ property $ \(B buf1) (B buf2) (B buf3) ->
    buf1 `mappend` (buf2 `mappend` buf3)
      == (buf1 `mappend` buf2) `mappend` buf3

  describe "empty" $ do
    it "should be null" $
      shouldSatisfy empty null

    it "should preserve invariants" $
      shouldSatisfy empty invariants

  describe "toText" $ do
    it "should undo fromText" $ property $ \(T txt) ->
      toText (fromText txt) == txt

  describe "fromText" $ do
    it "should undo toText" $ property $ \(B buf) ->
      fromText (toText buf) == buf

    it "should preserve invariants" $ property $ \(T txt) ->
      shouldSatisfy (fromText txt) invariants

  describe "toLazyText" $ do
    it "should undo fromLazyText" $ property $ \(LazyT txt) ->
      toLazyText (fromLazyText txt) == txt

  describe "fromLazyText" $ do
    it "should undo toLazyText" $ property $ \(B buf) ->
      fromLazyText (toLazyText buf) == buf

    it "should preserve invariants" $ property $ \(LazyT txt) ->
      shouldSatisfy (fromLazyText txt) invariants

  describe "splitAt" $ do
    it "should split buffer" $ property $ \(B buf) (NonNegative n) ->
      let (l, r) = splitAt n buf
      in l `append` r == buf

    it "should split buffer at specified point" $ property $
      \(B buf) (NonNegative n) ->
      let (l, r) = splitAt n buf
      in Text.length (toText l) == n || null r

    it "should preserve invariants" $ property $ \(B buf) (NonNegative n) ->
      let (l, r) = splitAt n buf
      in invariants l && invariants r

  describe "replace" $ do
    it "should replace the span with the text" $
      replace (Span 2 2) "!" (fromText "hello") `shouldBe` fromText "he!o"

    it "should be equivalent to the model" $ property $
      \(B buf) (T txt2) (NonNegative s) (NonNegative c) ->
      let model = mconcat [Text.take s txt1, txt2, Text.drop (s + c) txt1]
          txt1 = toText buf
      in replace (Span s c) txt2 buf `shouldBe` fromText model

newtype T = T {fromT :: Text}
  deriving (Show, Eq)

instance Arbitrary T where
  arbitrary = do
    str <- arbitrary
    return (T (Text.pack str))

newtype LazyT = LazyT Lazy.Text
  deriving (Show, Eq)

instance Arbitrary LazyT where
  arbitrary = do
    txts <- arbitrary
    return (LazyT . Lazy.Text.fromChunks . map fromT $ txts)

newtype B = B TextBuffer
  deriving (Show, Eq)

instance Arbitrary B where
  arbitrary = do
    LazyT txt <- arbitrary
    return (B (fromLazyText txt))

invariants :: TextBuffer -> Bool
invariants buf
  = noEmptyChunks
  && noSuccessiveSmallChunks
  && noLargeChunks
  where
  noEmptyChunks = go (toFingerTree buf)
    where
    go tree = case FingerTree.viewl tree of
      FingerTree.EmptyL -> True
      Chunk txt FingerTree.:< _
        | Text.null txt -> False
      _ FingerTree.:< rest -> go rest

  noSuccessiveSmallChunks = go Nothing (toFingerTree buf)
    where
    go maybe_l tree = case FingerTree.viewl tree of
      FingerTree.EmptyL -> True
      Chunk txt FingerTree.:< rest ->
        let l' = Text.length txt
        in case maybe_l of
          Just l
            | l' + l < minChunkLength
            -> False
          _ -> go (Just l') rest

  noLargeChunks = go (toFingerTree buf)
    where
    go tree = case FingerTree.viewl tree of
      FingerTree.EmptyL -> True
      Chunk txt FingerTree.:< _
        | Text.length txt > maxChunkLength -> False
      _ FingerTree.:< rest -> go rest

