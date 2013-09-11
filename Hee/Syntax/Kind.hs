module Hee.Syntax.Kind
  where

data Kind
  = Arr Kind Kind
  | Rho             -- ρ classifies stacks (similar to polymorphic row)
  | Star            -- * classifies types
  | Constraint
  deriving (Show, Eq)
