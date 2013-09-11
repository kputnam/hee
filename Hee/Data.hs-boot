module Hee.Data
  where

data Variant a
instance Eq a => Eq (Variant a)
instance Show a => Show (Variant a)
