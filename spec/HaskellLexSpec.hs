{-# LANGUAGE OverloadedStrings #-}

module HaskellLexSpec
( spec
)
where

import HaskellLex

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Test.Hspec

spec :: Spec
spec = describe "Haskell" $ do
  nextSpec
  tokensSpec

tokensSpec :: Spec
tokensSpec = describe "tokens" $ do
  it "should parse multiline comments" $ do
    fmap collectComments (parseOnly tokens "{- hello\nworld -}")
      `shouldBe` Right "{- hello\nworld -}"
  where
  collectComments :: [Token] -> Text
  collectComments = go Text.empty
  go res [] = res
  go res (White (Comment txt) : rest) = go (res `Text.append` txt) rest
  go res (White (CommentStart txt) : rest) = go (res `Text.append` txt) rest
  go res (White (CommentEnd txt) : rest) = go (res `Text.append` txt) rest
  go res (_ : rest) = go res rest

nextSpec :: Spec
nextSpec = describe "next" $ do
  it "should parse varid with small letters" $ do
    fmap snd (parseOnly (next initialState) "var")
      `shouldBe` Right (VarId [] "var")

  it "should parse varid with upper letters" $ do
    fmap snd (parseOnly (next initialState) "vAR")
      `shouldBe` Right (VarId [] "vAR")

  it "should parse varid with digits" $ do
    fmap snd (parseOnly (next initialState) "var1")
      `shouldBe` Right (VarId [] "var1")

  it "should parse varid with underscope" $ do
    fmap snd (parseOnly (next initialState) "var_")
      `shouldBe` Right (VarId [] "var_")

  it "should parse varid with a single quote mark" $ do
    fmap snd (parseOnly (next initialState) "var'")
      `shouldBe` Right (VarId [] "var'")

  it "should parse qualified varid" $ do
    fmap snd (parseOnly (next initialState) "Ab.Cd.var")
      `shouldBe` Right (VarId ["Ab", "Cd"] "var")

  it "should parse conid" $ do
    fmap snd (parseOnly (next initialState) "Con")
      `shouldBe` Right (ConId [] "Con")

  it "should parse qualified conid" $ do
    fmap snd (parseOnly (next initialState) "Ab.Cd.Con")
      `shouldBe` Right (ConId ["Ab", "Cd"] "Con")

  it "should parse varsym" $ do
    fmap snd (parseOnly (next initialState) "<$>")
      `shouldBe` Right (VarSym [] "<$>")

  it "should parse qualified varsym" $ do
    fmap snd (parseOnly (next initialState) "Ab.Cd.<$>")
      `shouldBe` Right (VarSym ["Ab", "Cd"] "<$>")

  it "should parse varcon" $ do
    fmap snd (parseOnly (next initialState) ":=")
      `shouldBe` Right (ConSym [] ":=")

  it "should parse qualified varcon" $ do
    fmap snd (parseOnly (next initialState) "Ab.Cd.:=")
      `shouldBe` Right (ConSym ["Ab", "Cd"] ":=")

  it "should parse specials" $ do
    fmap snd (parseOnly (next initialState) "[")
      `shouldBe` Right (Special LeftSquare)

  it "should parse reserved ops" $ do
    fmap snd (parseOnly (next initialState) "..")
      `shouldBe` Right (ReservedOp DotDot)

  it "should parse reserved ids" $ do
    fmap snd (parseOnly (next initialState) "import")
      `shouldBe` Right (ReservedId Import)

  it "should parse integer literal" $ do
    fmap snd (parseOnly (next initialState) "42")
      `shouldBe` Right (Lit (Integer 42))

  it "should parse float literal" $ do
    fmap snd (parseOnly (next initialState) "42.24")
      `shouldBe` Right (Lit (Float 42.24))

  it "should parse char literal" $ do
    fmap snd (parseOnly (next initialState) "'a'")
      `shouldBe` Right (Lit (Char 'a'))

  it "should parse string literal" $ do
    fmap snd (parseOnly (next initialState) "\"hello\"")
      `shouldBe` Right (Lit (String "hello"))

  it "should parse whitespace" $ do
    fmap snd (parseOnly (next initialState) " \t\r\n")
      `shouldBe` Right (White (WhiteSpace " \t\r\n"))

  it "should parse line comment" $ do
    fmap snd (parseOnly (next initialState) "-- comment")
      `shouldBe` Right (White (Comment "-- comment"))
