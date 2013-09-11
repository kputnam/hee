module Hee.Syntax.Literal
  ( Literal(..)
  ) where

import Data.Word
import Data.Text

data Literal
  = Byte     Word8
  | Short    Word16
  | Word     Word32
  | Long     Word64
  | Integer  Integer
  | String   Text
  | Char     Char
  | Float    Float
  | Double   Double
  deriving (Eq, Ord, Show, Read)
