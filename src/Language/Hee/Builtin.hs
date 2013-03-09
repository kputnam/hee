{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Builtin
  ( Builtin(..)
  , fromName
  , nameMap
  ) where

import qualified Data.Map as M
import Data.Text (Text)

data Builtin
  -- Stack operators
  = Id
  | Pop
  | Dup
  | Dup2
  | Dig
  | Swap
  | Bury
  -- Function operators
  | Quote
  | Compose
  | Unquote
  | Dip
  -- Numeric operators
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Quo
  | Exp
  | Round
  -- Boolean operators
  | If
  | Not
  | Or
  | And
  -- Comparison operators
  | Ne
  | Eq
  | Lt
  | Gt
  | Lte
  | Gte
  deriving (Eq, Show)

fromName :: Text -> Maybe Builtin
fromName = flip M.lookup nameMap

nameMap :: M.Map Text Builtin
nameMap = M.fromList
  [ ("id"       , Id)
  , ("pop"      , Pop)
  , ("dup"      , Dup)
  , ("dup2"     , Dup2)
  , ("dig"      , Dig)
  , ("swap"     , Swap)
  , ("bury"     , Bury)
  , ("quote"    , Quote)
  , ("compose"  , Compose)
  , ("unquote"  , Unquote)
  , ("dip"      , Dip)
  , ("+"        , Add)
  , ("-"        , Sub)
  , ("*"        , Mul)
  , ("/"        , Div)
  , ("%"        , Mod)
  , ("//"       , Quo)
  , ("^"        , Exp)
  , ("round"    , Round)
  , ("if"       , If)
  , ("not"      , Not)
  , ("or"       , Or)
  , ("and"      , And)
  , ("/="       , Ne)
  , ("=="       , Eq)
  , ("<"        , Lt)
  , (">"        , Gt)
  , ("<="       , Lte)
  , (">="       , Gte) ]
