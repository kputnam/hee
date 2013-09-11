module Hee.Syntax.Term
  where

import Hee.Syntax.Term

-- Singular and mutually-recursive bindings indexed by identifier
-- type. The parser need only construct one large recursive binding
-- which will be broken up into minimally connected components.
data Binding a
  = Non a (Term a)
  | Rec [(a, Term a)]
  deriving (Eq, Show)
