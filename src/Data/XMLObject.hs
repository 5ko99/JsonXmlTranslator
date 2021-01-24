module Data.XMLObject where

type Attribute = (String, String)

data TagElement = TagElement
  { name :: String,
    attributes :: [Attribute],
    children :: [XMLObject]
  }
  deriving (Show, Read)

instance Eq TagElement where
  (TagElement name1 _ _) == (TagElement name2 _ _) = name1 == name2

data XMLObject
  = Text String
  | Element TagElement
  deriving (Show, Read, Eq)
