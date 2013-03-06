{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Builtin
  ( Builtin(..)
  , fromName
  ) where

import qualified Data.Map as M
import Data.Text (Text)

data Builtin
  -- Stack operators
  = BId
  | BPop
  | BDup
  | BDup2
  | BDig
  | BSwap
  | BBury
  -- Function operators
  | BQuote
  | BCompose
  | BUnquote
  | BDip
  -- Numeric operators
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BQuo
  | BExp
  | BRound
  -- Boolean operators
  | BIf
  | BNot
  | BOr
  | BAnd
  -- Comparison operators
  | BNe
  | BEq
  | BLt
  | BGt
  | BLte
  | BGte
  deriving (Eq, Show)

fromName :: Text -> Maybe Builtin
fromName = flip M.lookup nameMap

nameMap :: M.Map Text Builtin
nameMap = M.fromList
  [ ("id"       , BId)
  , ("pop"      , BPop)
  , ("dup"      , BDup)
  , ("dup2"     , BDup2)
  , ("dig"      , BDig)
  , ("swap"     , BSwap)
  , ("bury"     , BBury)
  , ("quote"    , BQuote)
  , ("compose"  , BCompose)
  , ("unquote"  , BUnquote)
  , ("dip"      , BDip)
  , ("+"        , BAdd)
  , ("-"        , BSub)
  , ("*"        , BMul)
  , ("/"        , BDiv)
  , ("%"        , BMod)
  , ("//"       , BQuo)
  , ("^"        , BExp)
  , ("round"    , BRound)
  , ("if"       , BIf)
  , ("not"      , BNot)
  , ("or"       , BOr)
  , ("and"      , BAnd)
  , ("/="       , BNe)
  , ("=="       , BEq)
  , ("<"        , BLt)
  , (">"        , BGt)
  , ("<="       , BLte)
  , (">="       , BGte) ]
