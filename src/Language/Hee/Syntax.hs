module Language.Hee.Syntax
  where

import Data.Text

data Expression a
  = ExEmpty
  | ExName a
  | ExQuote (Expression a)
  | ExCompose (Expression a) (Expression a)
  | ExLiteral Literal
  deriving (Eq, Show)

data Literal
  = LiChar Char
  | LiString Text
  | LiInteger Integer Radix
  | LiFloat Float
  | LiBool Bool
  deriving (Eq, Show)

data Radix
  = Binary
  | Octal
  | Decimal
  | Hexadecimal
  deriving (Eq, Show)
