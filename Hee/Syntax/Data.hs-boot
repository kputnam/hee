module Hee.Syntax.Data
  where

data Constructor b a
instance (Eq b, Eq a) => Eq (Constructor b a)
instance (Show b, Show a) => Show (Constructor b a)
