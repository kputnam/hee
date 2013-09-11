module Hee.Syntax.Literal
  ( Literal(..)
  ) where

import Data.Binary
import Data.Word
import Data.Text
import Data.Text.Binary ()
import Control.Applicative

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

-- Binary serialization of literals
instance Binary Literal where
  put (Byte    x) = putWord8 0 *> put x
  put (Short   x) = putWord8 1 *> put x
  put (Word    x) = putWord8 2 *> put x
  put (Long    x) = putWord8 3 *> put x
  put (Integer x) = putWord8 4 *> put x
  put (String  x) = putWord8 5 *> put x
  put (Char    x) = putWord8 6 *> put x
  put (Float   x) = putWord8 7 *> put x
  put (Double  x) = putWord8 8 *> put x

  get = getWord8 >>= match
    where
      match 0 = Byte    <$> get
      match 1 = Short   <$> get
      match 2 = Word    <$> get
      match 3 = Long    <$> get
      match 4 = Integer <$> get
      match 5 = String  <$> get
      match 6 = Char    <$> get
      match 7 = Float   <$> get
      match 8 = Double  <$> get
      match b = fail $ "get Literal: unexpected tag " ++ show b
