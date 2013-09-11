module Hee.Syntax.Type
  ( Type(..)
  , Primitive(..)
  , Constructor(..)
  ) where

import Data.Text (Text)

import {-# SOURCE #-} qualified Hee.Syntax.Data as D
import Hee.Syntax.Kind

-- Types indexed by bound and free identifier types
data Type b a
  = Con (Constructor b a)     -- K      type constructor
  | Var a                     -- α      free type variable
  | Bnd b                     --        bound type variable
  | All b (Type b a)          -- ∀α:κ.γ polymorphic type
  | App (Type b a) (Type b a) -- τ τ    type application
  deriving (Eq, Show)

-- Type constructors parameterized over bound and free identifier types
data Constructor b a
  = Function  -- The function type constructor
    { name  :: Text   -- (->)
    , kind  :: Kind   -- ρ -> ρ -> ρ
    , arity :: Int    -- 2
    }
  | Empty     -- Type constructor for an empty stack
    { name  :: Text   -- $
    , kind  :: Kind   -- ρ
    , arity :: Int    -- 0
    }
  | Push      -- Type constructor for a non-empty stack
    { name  :: Text
    , kind  :: Kind   -- ρ -> * -> ρ
    , arity :: Int    -- 2
    }
  | Tuple     -- Infinite family of tuple type constructors: (), (a), (a,b), etc
    { name  :: Text
    , kind  :: Kind
    , arity :: Int
    , cons  :: D.Constructor b a -- corresponding data constructor
    }
  | Algebraic -- Algebraic type constructors
    { name  :: Text
    , kind  :: Kind
    , arity :: Int
    , cases :: [D.Constructor b a]
    }
  | Primitive -- Primitive types cannot be defined within Hee (e.g., int)
    { name  :: Text
    , kind  :: Kind
    , arity :: Int
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
