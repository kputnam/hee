module Hee.Syntax.Data
  ( Constructor(..)
  ) where

import Data.Text (Text)
import Hee.Syntax.Type hiding (Constructor)

data Constructor b a
  = Constructor
    { name    :: Text
    , tag     :: Int          -- sequntially numbered (per ADT)
    , args    :: [Type b a]   -- field types
    }
  deriving (Eq, Show)
