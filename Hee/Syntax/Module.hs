module Hee.Syntax.Module
  where

data Module
  = Module
    { name      :: Name
    , bindings  :: Map Name (Term Name)
    , terms     :: Map Name (Type Name) -- types of exported terms
    , types     :: Map Name (Type Void) -- exported types
    , data      :: Map Name (Type Void) -- exported data constructors
    }
  deriving (Eq, Show)
