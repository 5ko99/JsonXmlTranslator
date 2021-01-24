module Translation.ToJson (translate) where

import Data.JsonObject
import Data.XMLObject
import Helper
import Parsers.ParserUtils
import Parsers.XMLParser

--TODO: Fix converting to JSON. Need to use JSONString

attributeToJson :: Attribute -> Either String (String, JsonValue)
attributeToJson (name, val) = Right ('-' : name, JsonString val)

makePairs :: JsonValue -> Either String (String, JsonValue)
makePairs (JsonObject [(name, obj)]) = Right (name, obj)
makePairs str@(JsonString _) = Right ("", str)

rename :: [XMLObject] -> [XMLObject]
rename = helper 0
  where
    helper :: Int -> [XMLObject] -> [XMLObject]
    helper _ [] = []
    helper n (x@(Text _) : xs) = x : helper n xs
    helper n ((Element (TagElement name atr children)) : xs) =
      Element
        ( TagElement
            { name = show n ++ ":" ++ name,
              attributes = atr,
              children = children
            }
        ) :
      helper (n + 1) xs

translate :: XMLObject -> Either String JsonValue
--translate (Text t) = Right $ JsonObject [("Text", JsonString t)]
translate (Text t) = Right $ JsonString t
translate (Element (TagElement name atr children))
  | unique children = do
    attributeTranslated <- toEitherList (map attributeToJson atr)
    childrenTranslated <- toEitherList (map translate children)
    pairs <- toEitherList (map makePairs childrenTranslated)
    let combined = attributeTranslated ++ pairs
    return $ JsonObject [(name, JsonObject combined)]
  | name == "xml" = do
    arr <- toEitherList (map translate children)
    return $ JsonArray arr
  | otherwise = do
    let renamedChildren = rename children
    attributeTranslated <- toEitherList (map attributeToJson atr)
    childrenTranslated <- toEitherList (map translate renamedChildren)
    pairs <- toEitherList (map makePairs childrenTranslated)
    let combined = attributeTranslated ++ pairs
    return $ JsonObject [(name, JsonObject combined)]
