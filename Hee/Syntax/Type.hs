module Hee.Syntax.Type
  ( Type(..)
  , Kind
  , Sort
  , Constructor(..)
  , Primitive(..)
  ) where

import Data.Text (Text)

import {-# SOURCE #-} Hee.Data

-- Parameterized over variable type
data Type a
  = Con (Constructor a)   -- K      type constructor
  | Var a                 -- α      type variable
  | All a (Type a)        -- ∀α:κ.γ polymorphic type
  | App (Type a) (Type a) -- τ τ    type application
  deriving (Eq, Show)

type Kind = Type
type Sort = Type
type Name = Text

data Var a
  = TermV Name (Type a) -- term variable (not used)
  | TypeV Name (Kind a) -- type variable
  | KindV Name (Sort a) -- kind variable
  deriving (Eq, Show)

-- Parameterized over variable type
data Constructor a
  = Function  -- The function type constructor (->)
    { name  :: Name
    , kind  :: Kind a
    }
  | Empty     -- Type constructor for an empty stack ($)
    { name  :: Name
    , kind  :: Kind a
    }
  | Push      -- Type constructor for a non-empty stack
    { name  :: Name
    , kind  :: Kind a
    }
  | Tuple     -- Infinite family of tuple type constructors: (), (a), (a,b), etc
    { name  :: Name
    , kind  :: Kind a
    , vars  :: [a]
    , con   :: Variant a -- corresponding data constructor
    }
  | Algebraic -- Algebraic type constructors
    { name  :: Name
    , kind  :: Kind a
    , vars  :: [a]          -- kind and type vars used in the type constructor
    , cases :: [Variant a]  -- information about the data constructors of this type
    }
  | Synonym   -- Type synonyms
    { name  :: Name
    , kind  :: Kind a
    , vars  :: [a]            -- bound vars
    , rhs   :: Constructor a  -- expansion of the synonym
    }
  | Primitive -- Primitive types cannot be defined within Hee (e.g., int)
    { name  :: Name
    , kind  :: Kind a
    , prim  :: Primitive
    }
  deriving (Eq, Show)

data Primitive
  = Void
  -- | Ptr
  -- | WordW     -- unsized word-sized value
  | Word8     -- unsigned 8-bit value
  | Word16    -- unsigned 16-bit value
  | Word32    -- unsigned 32-bit value
  | Word64    -- unsigned 64-bit value
  -- | IntW      -- signed word-sized value
  -- | Int8      -- signed 8-bit value
  -- | Int16     -- signed 16-bit value
  -- | Int32     -- signed 32-bit value
  -- | Int64     -- signed 64-bit value
  -- | Addr      -- a pointer to a non-Hee value
  | Float
  | Double
  -- | Vector Int Primitive
  deriving (Eq, Show)
