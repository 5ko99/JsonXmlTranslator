module Translation.ToJson (translate) where

import Data.JsonObject
import Data.XMLObject
import Helper

attributeToJson :: Attribute -> Either String (String, JsonValue)
attributeToJson (name, val) = Right ('-' : name, JsonString val)

makePairs :: JsonValue -> Either String (String, JsonValue)
makePairs (JsonObject [(name, obj)]) = Right (name, obj)
makePairs _ = Left "Expected JsonValue!"

translate :: XMLObject -> Either String JsonValue
translate (Text t) = Right $ JsonObject [("Text", JsonString t)]
translate (Element (TagElement name atr children)) = do
  attributeTranslated <- toEitherList (map attributeToJson atr)
  childrenTranslated <- toEitherList (map translate children)
  pairs <- toEitherList (map makePairs childrenTranslated)
  let combined = attributeTranslated ++ pairs
  return $ JsonObject [(name, JsonObject combined)]