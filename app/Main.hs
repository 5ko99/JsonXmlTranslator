module Main where

import Data.Either
import Parsers.ParserUtils
import System.Environment (getArgs)
import Translator (toJSON, toXML)

{--
  Arguments of main:
  first : Type of translate: 0 - to JSON/ 1- to XML
  second: Path to source file
  third: Path to the new file
--}

fromFileToFile :: [Char] -> FilePath -> FilePath -> IO ()
fromFileToFile typeCon from to = do
  file <- readFile from
  let translated = if typeCon == "0" then toJSON file else toXML file
      result :: String
      result =
        if isRight translated
          then fromRight "" translated
          else show $ fromLeft (ParserError "Error!" (-1)) translated
  writeFile to result

-- 0 - to JSON
-- 1 - to XML
main :: IO ()
main = do
  [typeCon, from, to] <- getArgs
  fromFileToFile typeCon from to