module Hee.Syntax.Literal
  ( Literal(..)
  ) where

import Data.Binary
import Control.Applicative
import Control.Monad

data Literal
  = Int      !Int32
  | Long     !Int64
  | Short    !Int16
  | Byte     !Int8
  | String   !Text
  | Char     !Char
  | Float    !Float
  | Double   !Double
  deriving (Eq, Ord, Show, Read)

-- Binary serialization of literals
instance Binary Literal where
  put (Int    x) = putWord8 0 *> put i
  put (Long   x) = putWord8 1 *> put i
  put (Short  x) = putWord8 2 *> put i
  put (Byte   x) = putWord8 3 *> put i
  put (String x) = putWord8 4 *> put i
  put (Char   x) = putWord8 5 *> put i
  put (Float  x) = putWord8 6 *> put i
  put (Double x) = putWord8 7 *> put i

  get = getWord8 >>= match
    where
      match 0 = Int    <$> get
      match 1 = Long   <$> get
      match 2 = Short  <$> get
      match 3 = Byte   <$> get
      match 4 = String <$> get
      match 5 = Char   <$> get
      match 6 = Float  <$> get
      match 7 = Double <$> get
      match b = fail $ "get Literal: unexpected tag " ++ show b
