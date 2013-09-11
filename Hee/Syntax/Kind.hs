module Hee.Syntax.Kind
  where

-- There is no binder for vars?
data Kind -- a
  -- = Var a
  -- | Arr (Kind a) (Kind a)
  = Arr Kind Kind
  | Rho
  | Star
  | Constraint
  deriving (Show, Eq)


