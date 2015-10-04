
module Main
( main
)
where

import qualified TextBufferSpec
import qualified TokenBufferSpec
import qualified HaskellLexSpec

import Test.Hspec.Runner as Runner

main :: IO ()
main = Runner.hspec $ do
  TextBufferSpec.spec
  TokenBufferSpec.spec
  HaskellLexSpec.spec
