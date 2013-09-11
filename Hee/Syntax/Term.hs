module Hee.Syntax.Term
  where

import Hee.Syntax.Literal

data Bind a
  = Non a (Term a)
  | Rec [(a, Term a)]
  deriving (Eq, Show)

data Term a
  = Empty
  | Name a
  | Quote (Term a)
  | Compose (Term a) (Term a)
  | Literal Literal
  deriving (Eq, Show)
