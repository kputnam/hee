module Language.Hee.Syntax
  where

import Data.Text

data Bind a
  = Non a (Expr a)
  | Rec [(a, Expr a)]
  deriving (Eq, Show)

data Expr a
  = Empty
  | Name a
  | Quote (Expr a)
  | Compose (Expr a) (Expr a)
  | Literal Literal
  deriving (Eq, Show)

data Literal
  = Chr Char
  | Str Text
  | Int Radix Integer
  | Rat Rational
  deriving (Eq, Show)

data Radix
  = Bin
  | Oct
  | Hex
  | Dec
  deriving (Eq, Show)
