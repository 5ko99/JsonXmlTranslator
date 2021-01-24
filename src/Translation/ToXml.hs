module Translation.ToXml(translate) where

import Data.JsonObject
import Data.XMLObject
import Helper

jsonListToXml :: [(String, JsonValue)] -> Either String [XMLObject]
jsonListToXml [] = Right []
jsonListToXml lst@((name, json@(JsonObject pair)) : xs) = do
  headAttributes <- getAttributes pair
  headChildren <- jsonToXmlObjL json
  let translatedHead = Element (TagElement name headAttributes headChildren)
  translatedTail <- jsonListToXml xs
  return $ translatedHead : translatedTail
jsonListToXml ((name, json) : xs)
  | name == "Text" = do
    text <- getString json
    translatedTail <- jsonListToXml xs
    return $ Text text : translatedTail
  | head name /= '-' = do
    headChildren <- jsonToXmlObjL json
    let headElement = Element (TagElement name [] headChildren)
    translatedTail <- jsonListToXml xs
    return $ headElement : translatedTail -- Skipping attributes
  | otherwise = jsonListToXml xs

getString :: JsonValue -> Either String String
getString (JsonString str) = Right str
getString _ = Left "Expected JsonString!"

jsonArrToXml :: [JsonValue] -> Either String [XMLObject]
jsonArrToXml arr = do
  let mapped = toEitherList (map jsonToXmlObjL arr)
  concat <$> mapped

getAttributes :: [(String, JsonValue)] -> Either String [Attribute]
getAttributes [] = Right []
getAttributes ((name, JsonString str) : xs)
  | head name == '-' = do
    parsedTail <- getAttributes xs
    return $ (drop 1 name, str) : parsedTail
  | otherwise = getAttributes xs
getAttributes (_ : xs) = getAttributes xs

jsonToXmlObjL :: JsonValue -> Either String [XMLObject]
jsonToXmlObjL JsonNull = Right [Text "null"]
jsonToXmlObjL (JsonString str) = Right [Text str]
jsonToXmlObjL (JsonBool b) = Right [Text $ show b]
jsonToXmlObjL (JsonNumber n) = Right [Text $ show n]
jsonToXmlObjL (JsonArray arr) = jsonArrToXml arr
jsonToXmlObjL (JsonObject arr) = jsonListToXml arr

translate :: JsonValue -> Either String XMLObject
translate (JsonObject [(name, json@(JsonObject jv))]) = do
  parsedAttributes <- getAttributes jv
  parsedChildren <- jsonToXmlObjL json
  return $ Element (TagElement name parsedAttributes parsedChildren)
translate json = do
  parsedChildren <- jsonToXmlObjL json
  return $ Element (TagElement "xml" [] parsedChildren)