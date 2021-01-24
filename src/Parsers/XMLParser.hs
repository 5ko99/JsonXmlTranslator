{-# LANGUAGE NamedFieldPuns #-}

module Parsers.XMLParser (parse) where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative ((*>), (<*), (<*>)),
    (<$>),
  )
import Data.Char ()
import Data.XMLObject
  ( Attribute,
    TagElement (TagElement),
    XMLObject (..),
  )
import Parsers.ParserUtils
  ( Parser (runParser),
    ParserResult,
    abortParser,
    char,
    closingName,
    commentParser,
    noArrow,
    string,
    stringLiteral,
    tag,
    text,
    ws,
  )
import Prelude hiding (span)

attributeParser :: Parser Attribute
attributeParser =
  (\_ name _ param _ -> (name, param))
    <$> ws
    <*> tag
    <*> char '='
    <*> stringLiteral
    <*> ws

closingTagParser :: String -> Parser String
closingTagParser name = do
  ws
  string "</"
  closingName <- closingName
  char '>'
  ws
  if name == closingName
    then return name
    else abortParser "Error!" 0

tagParser :: Parser XMLObject
tagParser = do
  name <- many commentParser *> ws *> char '<' *> ws *> text <* ws
  attributes <- many attributeParser
  ws <* char '>' <* ws <* many commentParser
  children <- many xmlParser
  ws *> many commentParser *> closingTagParser name <* ws <* many commentParser
  return $ Element $ TagElement name attributes children

textParser :: Parser XMLObject
textParser =
  Text
    <$> (ws *> noArrow <* ws)

xmlParser :: Parser XMLObject
xmlParser = tagParser <|> textParser

parse :: String -> ParserResult XMLObject
parse  = runParser xmlParser