module Hee.Data
  ( Variant(..)
  , Tag
  ) where

import Data.Text (Text)
import Hee.Syntax.Type

type Tag  = Int
type Name = Text

data Variant a
  = Variant
    { name    :: Name
    , tag     :: Tag
    , args    :: [Type a]
    }
  deriving (Eq, Show)
