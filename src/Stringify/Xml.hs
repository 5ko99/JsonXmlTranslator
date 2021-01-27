module Stringify.Xml (toString) where

import Data.XMLObject
  ( Attribute,
    TagElement (TagElement),
    XMLObject (..),
  )
import Helper (myMap, printSpaces, toEitherList)

attributeToString :: Attribute -> String
attributeToString (a, b) = ' ' : a ++ "=" ++ "\"" ++ b ++ "\""

attributesToStrings :: [Attribute] -> [String]
attributesToStrings = map attributeToString

attributesToString :: [Attribute] -> String
attributesToString = concat . attributesToStrings

listOfTextsToString :: Int -> String -> [Attribute] -> [XMLObject] -> Either String String
listOfTextsToString _ _ _ [] = Right []
listOfTextsToString spaces name attributes (Text x : xs) = do
  xsResult <- listOfTextsToString spaces name attributes xs
  Right $
    printSpaces spaces
      ++ '<' :
    name
      ++ attributesToString attributes
      ++ ">"
      ++ x
      ++ "</"
      ++ name
      ++ ">\n"
      ++ xsResult
listOfTextsToString _ _ _ (Element _ : _) = Left "Error:Expected XMLObject Text!"

tagElementToString :: Int -> TagElement -> Either String String
tagElementToString n (TagElement str atr [Text text]) = do
  -- case for only one element (Text as a child) no new lines
  return $
    printSpaces n
      ++ "<"
      ++ str
      ++ attributesToString atr
      ++ ">"
      ++ text
      ++ "</"
      ++ str
      ++ ">\n"
tagElementToString n (TagElement str atr children@(Text _ : _)) = do
  -- case for array of text elements
  listOfTextsToString n str atr children
tagElementToString n (TagElement str atr children) = do
  others <- toEitherList (myMap xmlToString (n + 1) children)
  return $
    printSpaces n
      ++ "<"
      ++ str
      ++ attributesToString atr
      ++ ">\n"
      ++ concat others
      ++ printSpaces n
      ++ "</"
      ++ str
      ++ ">\n"

xmlToString :: Int -> XMLObject -> Either String String
xmlToString _ (Text text) = Right text
xmlToString n (Element el) = tagElementToString n el

toString :: XMLObject -> Either String String
toString = xmlToString 0