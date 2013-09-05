{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Hee.Check
  where

import Language.Hee.Type
import Language.Hee.Syntax

import Control.Applicative
import Control.Monad.State
import Data.Text (Text)
import Data.Monoid
import Data.Map

import qualified Data.Text as T

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
  mempty
    = Context mempty mempty

  mappend a b
    = Context constraints' assumptions'
    where
      constraints' = constraints a <> constraints b
      assumptions' = assumptions a <> assumptions b

freshVar :: State Int (Type Text)
freshVar
  = do n <- get
       modify succ
       return . Var $ letter n `T.cons` ticks n
  where
    alphabet = "abcdefghijklmnopqurstuvwxyz"
    letter n = alphabet !! (n `rem`  length alphabet)
    ticks  n = T.replicate (n `quot` length alphabet) "'"

-- Generalizing Hindley-Milner Type Inference Algorithms
--   B. Heeren, J. Hage, D. Swierstra (2002)
mkConstraints :: Expr Text -> State Int (Type Text, Context Text)
mkConstraints Empty
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
  = do var       <- freshVar
       (ta, ca)  <- mkConstraints a
       return (var `tyFun` (var `tyStack` ta), ca)
mkConstraints (Compose a b)
  -- Composing two terms yields `S -> U` if the first
  -- term unifies with `S -> T` and the second `T -> U`.
  = do varS      <- freshVar
       varT      <- freshVar
       varU      <- freshVar
       (ta, ca)  <- mkConstraints a
       (tb, cb)  <- mkConstraints b
       let cc = Context [ Unify ta (varS `tyFun` varT)
                        , Unify tb (varT `tyFun` varU) ] mempty
       return (varS `tyFun` varU, ca <> cb <> cc)
mkConstraints (Literal (Chr _))
  = do var <- freshVar
       return (var `tyFun` (var `tyStack` tyChr), mempty)
mkConstraints (Literal (Str _))
  = do var <- freshVar
       return (var `tyFun` (var `tyStack` tyStr), mempty)
mkConstraints (Literal (Int _ _))
  = do var <- freshVar
       return (var `tyFun` (var `tyStack` tyInt), mempty)
mkConstraints (Literal (Rat _))
  = do var <- freshVar
       return (var `tyFun` (var `tyStack` tyRat), mempty)

------------------------------------------------------------------------------

-- Types for literal values
tyChr, tyStr, tyInt, tyRat :: Type a
tyChr = Lit "chr"
tyStr = Lit "str"
tyInt = Lit "int"
tyRat = Lit "rat"

-- Type of function from first arg to second
tyFun :: Type a -> Type a -> Type a
tyFun domain codomain
  = App (App tcFun domain) codomain
  where
    -- Type constructor for functions
    tcFun :: Type a
    tcFun
      = Con $ Function "->" (Lit "* -> * -> *")

-- Type of non-empty stack with second arg pushed onto the first
tyStack :: Type Text -> Type Text -> Type Text
tyStack rest top
  = App (App tcPair rest) top
  where
    -- Type constructor for non-empty stack
    tcPair :: Type Text
    tcPair
      = Con $ Algebraic "*" (Lit "* -> * -> *") ["S", "a"] []

-- Type of empty stack
tyEmpty :: Type Text
tyEmpty
  = Lit "%"
