{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Hee.Inference.Constraint
  where

import Control.Applicative
import Control.Monad.State
import Data.Text (Text)
import Data.Monoid
import Data.Map
import qualified Data.Text as X

import Hee.Syntax.Kind
import Hee.Syntax.Type    as T
import Hee.Syntax.Term    as E
import Hee.Syntax.Literal as L

data Constraint b a
  = Unify (Type b a) (Type b a) -- two types should be unified
  | Match (Type b a) (Type b a) -- first is an instance of second
  deriving (Eq, Show)

data Context b a
  = Context
    { constraints :: [Constraint b a]
    , assumptions :: Map Text [Type b a]
      -- Contrary to the standard type environment Γ in Hindley-
      -- Milner inference, there may be multiple assumptions for
      -- a given variable.
    }
  deriving (Eq, Show)

instance Ord b => Monoid (Context b a) where
  mempty
    = Context mempty mempty

  mappend a b
    = Context constraints' assumptions'
    where
      constraints' = mappend (constraints a) (constraints b)
      assumptions' = unionWith (++) (assumptions a) (assumptions b)

freshVar :: State Int (Type Int Text)
freshVar
  = do n <- get
       modify succ
       return . Var $ letter n `X.cons` ticks n
  where
    alphabet = "abcdefghijklmnopqurstuvwxyz"
    letter n = alphabet !! (n `rem`  length alphabet)
    ticks  n = X.replicate (n `quot` length alphabet) "'"

-- Generalizing Hindley-Milner Type Inference Algorithms
--   B. Heeren, J. Hage, D. Swierstra (2002)
mkConstraints :: Term Text -> State Int (Type Int Text, Context Int Text)
mkConstraints E.Empty
  -- The empty term has any type and doesn't add
  -- any new assumptions nor add new constraints
  = (, mempty) <$> freshVar
mkConstraints (Name a)
  -- Referencing an identifier just creates a fresh
  -- variable and adds it to the map of assumptions
  = f <$> freshVar
  where
    f v = (v, Context mempty (singleton a [v]))
mkConstraints (Quote a)
  -- Quoting an expression promotes its type `ta` to
  -- the type `S -> S ta` where S is a fresh variable
  = do s        <- freshVar
       (ta, ca) <- mkConstraints a
       return (s `tyFun` (s `tyStack` ta), ca)
mkConstraints (Compose a b)
  -- Composing two terms yields `S -> U` if the first
  -- term unifies with `S -> T` and the second `T -> U`.
  = do s        <- freshVar
       t        <- freshVar
       u        <- freshVar
       (ta, ca) <- mkConstraints a
       (tb, cb) <- mkConstraints b
       let cc    = Context [ Unify ta (s `tyFun` t)
                           , Unify tb (t `tyFun` u) ] mempty
       return (s `tyFun` u, ca <> cb <> cc)
mkConstraints (Literal x)
  = pushType (Con (litType x)) <$> freshVar
  where
    litType (L.Byte    _) = Primitive { name = "byte"   , kind = Star, arity = 1, prim = T.Word8  }
    litType (L.Short   _) = Primitive { name = "short"  , kind = Star, arity = 1, prim = T.Word16 }
    litType (L.Word    _) = Primitive { name = "word"   , kind = Star, arity = 1, prim = T.Word32 }
    litType (L.Long    _) = Primitive { name = "long"   , kind = Star, arity = 1, prim = T.Word64 }
    litType (L.Float   _) = Primitive { name = "float"  , kind = Star, arity = 1, prim = T.Float  }
    litType (L.Double  _) = Primitive { name = "double" , kind = Star, arity = 1, prim = T.Double }
    litType (L.Integer _) = error "todo"
    litType (L.String  _) = error "todo"
    litType (L.Char    _) = error "todo"

pushType :: Type Int Text -> Type Int Text -> (Type Int Text, Context Int Text)
pushType t rest
  = (rest `tyFun` (rest `tyStack` t), mempty)

-- Built-in types and type constructors
------------------------------------------------------------------------------

-- Type of function from first arg to second
tyFun :: Type b a -> Type b a -> Type b a
tyFun domain codomain
  = App (App tcFun domain) codomain
  where
    -- Type constructor for functions
    tcFun :: Type b a
    tcFun
      = Con $ Function "->" (Arr Rho Rho) 2

-- Type of non-empty stack with second arg pushed onto the first
tyStack :: Type b a -> Type b a -> Type b a
tyStack bottom top
  = App (App (Con (Push { name = ":", kind = Arr Rho Star, arity = 2 })) bottom) top

-- Type of empty stack
tyEmpty :: Type b a
tyEmpty
  = Con (T.Empty { name = "$", kind = Rho, arity = 0 })
