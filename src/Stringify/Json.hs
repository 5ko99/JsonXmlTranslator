module Stringify.Json (toString) where

import Data.JsonObject (JsonValue (..))
import Helper

jsonObjectToString :: Int -> [(String, JsonValue)] -> Either String String
jsonObjectToString _ [] = Right ""
-- array is the only element in val
jsonObjectToString n ((str, val@(JsonArray arr)) : xs) = do
  children <- jsonValueToString (n + 1) False val
  stringifyTail <- jsonObjectToString n xs
  let name = '"' : str ++ "\": [\n"
  return $
    printSpaces n
      ++ name
      ++ children
      ++ "\n"
      ++ printSpaces n
      ++ "]"
      ++ if isEmpty xs
        then ""
        else
          "," ++ "\n"
            ++ stringifyTail
-- not empty tail for that comma at the end
jsonObjectToString n ((str, val@(JsonObject _)) : xs) = do
  children <- jsonValueToString (n + 1) False val
  stringifyTail <- jsonObjectToString n xs
  let name = '"' : str ++ "\": {\n"
  return $
    printSpaces n
      ++ name
      ++ children
      ++ printSpaces n
      ++ "}"
      ++ if isEmpty xs
        then "\n"
        else
          ",\n"
            ++ stringifyTail
-- not array and not JsonObj- No {}
jsonObjectToString n ((str, val) : xs) = do
  children <- jsonValueToString (n + 1) False val
  stringifyTail <- jsonObjectToString n xs
  let name = '"' : str ++ "\": "
  return $
    printSpaces n
      ++ name
      ++ children
      ++ if isEmpty xs
        then "\n"
        else
          ",\n"
            ++ stringifyTail

-- if flag is true then this function is invoked by jsonArray and we need spaces
-- if it's false then we don't need spaces at first data types
jsonValueToString :: Int -> Bool -> JsonValue -> Either String String
jsonValueToString n flag JsonNull
  | flag = Right $ ",\n" ++ printSpaces n ++ "null"
  | otherwise = Right "null"
jsonValueToString n flag (JsonBool b)
  | flag = Right $ ",\n" ++ printSpaces n ++ show b
  | otherwise = Right $ show b
jsonValueToString n flag (JsonNumber num)
  | flag = Right $ ",\n" ++ printSpaces n ++ show num
  | otherwise = Right $ show num
jsonValueToString n flag (JsonString str)
  | flag = Right $ ",\n" ++ printSpaces n ++ "\"" ++ str ++ "\""
  | otherwise = Right $ '"' : str ++ "\""
jsonValueToString n _ (JsonArray arr) = do
  stringArr <- toEitherList (myMapForJson jsonValueToString n True arr)
  let concatenated = concat stringArr
  return $ drop 2 concatenated
jsonValueToString n _ (JsonObject obj) = do
  stringifiedObj <- jsonObjectToString n obj
  return $ stringifiedObj

toString :: JsonValue -> Either String String
toString val = do
  res <- jsonValueToString 1 False val
  return $ "{\n" ++ res ++ "}"