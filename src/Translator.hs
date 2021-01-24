module Translator where

import Data.Either (fromLeft, fromRight, isLeft)
import Data.JsonObject
import Data.XMLObject
import qualified Parsers.JsonParser as JSONParser
import qualified Parsers.ParserUtils as ParserUtils
import qualified Parsers.XMLParser as XMLParser
import qualified Stringify.Json as JSON
import qualified Stringify.Xml as XML
import qualified Translation.ToJson as ToJSON
import qualified Translation.ToXml as ToXML

toXML :: String -> Either ParserUtils.ParserError String
toXML json = do
  parsedJSON <- JSONParser.parse json
  let jsonValue = ParserUtils.getValue parsedJSON
      xmlObj = ToXML.translate jsonValue
      result = xmlObj >>= XML.toString
  return $ if isLeft result then fromLeft "" result else fromRight "" result

toJSON :: String -> Either ParserUtils.ParserError String
toJSON xml = do
  parsedXML <- XMLParser.parse xml
  let xmlObject = ParserUtils.getValue parsedXML
      jsonValue = ToJSON.translate xmlObject
      result = jsonValue >>= JSON.toString
  return $ if isLeft result then fromLeft "" result else fromRight "" result

-- Save using fromLeft and fromRight because the two types of Either are string, so we are sure that we return
-- string. Also it's unnecessary for toString to return Either because every object can be represented as a string
-- but I do not have time to change it now :) (Last minute codding)
