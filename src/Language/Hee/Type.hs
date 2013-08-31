module Language.Hee.Type
  ( Type(..)
  , Kind
  , Sort
  , Constructor(..)
  , Primitive(..)
  , arity
  ) where

import Data.Text (Text)

import {-# SOURCE #-} Language.Hee.Data

data Type a
  = Con (Constructor a)   -- type constructor
  | Lit Name              -- type literal
  | Var a                 -- type variable
  | Lam a (Type a)        -- polymorphic type
  | App (Type a) (Type a) -- constructor application
  deriving (Eq, Show)

type Kind = Type
type Sort = Type
type Name = Text

data Constructor a
  = Function -- The function type constructor (->)
    { name  :: Name
    , kind  :: Kind a
    }
  | Algebraic -- Algebraic type constructors
    { name  :: Name
    , kind  :: Kind a
    , vars  :: [a]          -- kind and type vars used in the type constructor
    , cases :: [Variant a]  -- information about the data constructors of this type
    }
  | Tuple -- Infinite family of tuple type constructors: (), (a), (a,b), etc
    { name  :: Name
    , kind  :: Kind a
    , vars  :: [a]
    , con   :: Variant a -- corresponding data constructor
    }
  | Synonym -- Type synonyms
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

arity :: Constructor a -> Int
arity (Function {})             = 2
arity (Algebraic { vars = v })  = length v
arity (Tuple { vars = v })      = length v
arity (Synonym { vars = v })    = length v
arity (Primitive {})            = 0

data Primitive
  = Void
  | Ptr
  | WordW     -- unsized word-sized value
  | Word8     -- unsigned 8-bit value
  | Word16    -- unsigned 16-bit value
  | Word32    -- unsigned 32-bit value
  | Word64    -- unsigned 64-bit value
  | IntW      -- signed word-sized value
  | Int8      -- signed 8-bit value
  | Int16     -- signed 16-bit value
  | Int32     -- signed 32-bit value
  | Int64     -- signed 64-bit value
  | Addr      -- a pointer to a non-Hee value
  | Float
  | Double
  | Vector Int Primitive
  deriving (Eq, Show)
