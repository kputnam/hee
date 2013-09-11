module Hee.Syntax.Name
  where

-- Fully qualified top level name
data Name
  = Name
    { name        :: Text
    , qualifiers  :: [Text]
    , digest      :: ByteString
    }

instance Show Name where
  show (Name n qs _) = show n

instance Eq Name where
  (==) = (==) `on` digest

instance Ord Name where
  compare = compare `on` digest
