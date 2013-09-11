module Hee.Syntax.Term
  where

import Hee.Syntax.Literal

-- Terms indexed by identifier type. There are no binders within
-- the term language, but each module defines top-level bindings
-- that can be imported into other modules.
--
-- The parser will construct @Term Text@ values which contain only
-- names of referenced identifiers. In a later phase, these @Text@
-- names are resolved to unique identifiers which carry types and
-- other information.
data Term a
  = Empty
  | Name a
  | Quote (Term a)
  | Compose (Term a) (Term a)
  | Literal Literal
  deriving (Eq, Show)
