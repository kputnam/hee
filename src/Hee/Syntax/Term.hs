module Hee.Syntax.Term
  where

import Data.Text
import Hee.Syntax.Literal

data Bind a
  = Non a (Expr a)
  | Rec [(a, Expr a)]
  deriving (Eq, Show)

data Term a
  = Empty
  | Name a
  | Quote (Expr a)
  | Compose (Expr a) (Expr a)
  | Literal Literal
  deriving (Eq, Show)
