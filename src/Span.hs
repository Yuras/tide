
module Span
where

data Span = Span
  { start :: Int
  , count :: Int
  }
  deriving (Show, Eq)
