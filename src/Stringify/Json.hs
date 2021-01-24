module Stringify.Json (toString) where

import Data.JsonObject (JsonValue (..))
import Helper

jsonObjectToString :: Int -> Bool -> [(String, JsonValue)] -> Either String String
jsonObjectToString _ _ [] = Right ""
-- array is the only element in val
jsonObjectToString n flag ((str, val@(JsonArray arr)) : xs) = do
  children <- jsonValueToString (n + 1) False val
  stringifyTail <- jsonObjectToString n flag xs
  let name = '"' : str ++ "\": [\n"
  return $
    printSpaces n
      ++ name
      ++ children
      ++ "\n"
      ++ printSpaces n
      ++ "]"
      ++ if isEmpty xs && not flag
        then ""
        else
          "," ++ "\n"
            ++ stringifyTail
-- not empty tail for that comma at the end
jsonObjectToString n flag ((str, val@(JsonObject _)) : xs) = do
  children <- jsonValueToString (n + 1) False val
  --stringifyTail <- jsonObjectToString n flag xs
  stringifyTail <- jsonObjectToString n flag xs
  let name = '"' : str ++ "\": {\n"
  return $
    printSpaces n
      ++ name
      ++ children
      ++ printSpaces n
      ++ "}"
      ++ if isEmpty xs && not flag
        then "\n"
        else
          ",\n"
            ++ stringifyTail
-- not array and not JsonObj- No {}
jsonObjectToString n flag ((str, val) : xs) = do
  children <- jsonValueToString (n + 1) False val
  stringifyTail <- jsonObjectToString n flag xs
  let name = '"' : str ++ "\": "
  return $
    printSpaces n
      ++ name
      ++ children
      ++ if isEmpty xs && not flag
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
jsonValueToString n flag (JsonObject obj) = do jsonObjectToString n flag obj

toString :: JsonValue -> Either String String
toString val@(JsonArray lst) = do
  let len = length lst
      b = take (len -1) lst
      c = JsonArray b
      d = last lst
  res <- jsonValueToString 1 True c
  res1 <- jsonValueToString 1 False d
  return $ "{\n" ++ res ++ res1 ++ "}"
toString val = do
  res <- jsonValueToString 1 False val
  return $ "{\n" ++ res ++ "}"