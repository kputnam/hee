module Language.Hee.Data
  ( Variant(..)
  , Tag
  ) where

import Data.Text (Text)
import Language.Hee.Type

type Tag  = Int
type Name = Text

data Variant a
  = Variant
    { name    :: Name
    , tag     :: Tag
    , args    :: [Type a]
    }
  deriving (Eq, Show)
