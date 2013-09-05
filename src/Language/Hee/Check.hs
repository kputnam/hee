{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Hee.Check
  where

import Language.Hee.Type
import Language.Hee.Syntax

import Data.Map
import Data.Monoid
import Control.Applicative

data Constraint a
  = Unify (Type a) (Type a) -- two types should be unified
  | Match (Type a) (Type a) -- first is an instance of second
  deriving (Eq, Show)

data Context a
  = Context
    { constraints :: [Constraint a]
    , assumptions :: Map a [Type a]
      -- Contrary to the standard type environment Γ in Hindley-
      -- Milner inference, there may be multiple assumptions for
      -- a given variable.
    }
  deriving (Eq, Show)

instance Ord a => Monoid (Context a) where
  mempty      = Context mempty mempty
  mappend a b = Context constraints' assumptions'
    where
      constraints' = constraints a `mappend` constraints b
      assumptions' = assumptions a `mappend` assumptions b

freshVar :: Monad m => m (Type a)
freshVar = return (Lit "F")

tyChr, tyStr, tyInt, tyRat :: Type a
tyChr = Lit "chr"
tyStr = Lit "str"
tyInt = Lit "int"
tyRat = Lit "rat"

-- Generalizing Hindley-Milner Type Inference Algorithms
--   B. Heeren, J. Hage, D. Swierstra (2002)
mkConstraints :: (Ord a, Applicative m, Monad m) => Expr a -> m (Type a, Context a)
mkConstraints Empty
  -- The empty term has any type and doesn't add
  -- any new assumptions nor add new constraints
  = (, mempty) <$> freshVar
mkConstraints (Name a)
  -- Referencing an identifier just creates a fresh
  -- variable and adds it to the map of assumptions
  = f <$> freshVar
  where f v = (v, Context mempty (singleton a [v]))
mkConstraints (Quote a)
  -- Quoting an expression promotes its type `ta` to
  -- the type `S -> S ta` where S is a fresh variable
  = do var       <- freshVar
       (ta, ca)  <- mkConstraints a
       return (undefined, ca)
mkConstraints (Compose a b)
  -- Composing two terms yields `S -> U` if the first
  -- term unifies with `S -> T` and the second `T -> U`.
  = do varS      <- freshVar
       varT      <- freshVar
       varU      <- freshVar
       (ta, ca)  <- mkConstraints a
       (tb, cb)  <- mkConstraints b
       -- varS -> varU
       -- + Unify ta (varS -> varT)
       -- + Unify tb (varT -> varU)
       return undefined
-- Literals do not produce assumptions or constraints
mkConstraints (Literal (Chr _))   = pure (tyChr, mempty)
mkConstraints (Literal (Str _))   = pure (tyStr, mempty)
mkConstraints (Literal (Int _ _)) = pure (tyInt, mempty)
mkConstraints (Literal (Rat _))   = pure (tyRat, mempty)
