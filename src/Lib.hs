module Lib
  ( someFunc,
    XMLParser.parse,
  )
where

import Data.Either
import Data.JsonObject
import Data.XMLObject
import qualified Parsers.JsonParser as JSONParser
import qualified Parsers.XMLParser as XMLParser
import qualified Stringify.Json as JSON
import qualified Stringify.Xml as XML

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--test
testXmlTOString :: IO ()
testXmlTOString = do
  writeFile "xml1.xml" $
    fromRight [] $
      XML.toString $
        Element
          ( TagElement
              { name = "body",
                attributes = [],
                children =
                  [ Element
                      ( TagElement
                          { name = "p",
                            attributes = [],
                            children = [Text "test"]
                          }
                      )
                  ]
              }
          )

test2 = do
  writeFile "testXML2.xml" $
    fromRight [] $
      XML.toString $
        Element (TagElement {name = "catalog", attributes = [], children = [Element (TagElement {name = "book", attributes = [("id", "bk101")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Gambardella, Matthew"]}), Element (TagElement {name = "title", attributes = [], children = [Text "XML Developer's Guide"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Computer"]}), Element (TagElement {name = "price", attributes = [], children = [Text "44.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-10-01"]}), Element (TagElement {name = "description", attributes = [], children = [Text "An in-depth look at creating applications \n      with XML."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk102")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Ralls, Kim"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Midnight Rain"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Fantasy"]}), Element (TagElement {name = "price", attributes = [], children = [Text "5.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-12-16"]}), Element (TagElement {name = "description", attributes = [], children = [Text "A former architect battles corporate zombies, \n      an evil sorceress, and her own childhood to become queen \n      of the world."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk103")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Corets, Eva"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Maeve Ascendant"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Fantasy"]}), Element (TagElement {name = "price", attributes = [], children = [Text "5.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-11-17"]}), Element (TagElement {name = "description", attributes = [], children = [Text "After the collapse of a nanotechnology \n      society in England, the young survivors lay the \n      foundation for a new society."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk104")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Corets, Eva"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Oberon's Legacy"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Fantasy"]}), Element (TagElement {name = "price", attributes = [], children = [Text "5.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2001-03-10"]}), Element (TagElement {name = "description", attributes = [], children = [Text "In post-apocalypse England, the mysterious \n      agent known only as Oberon helps to create a new life \n      for the inhabitants of London. Sequel to Maeve \n      Ascendant."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk105")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Corets, Eva"]}), Element (TagElement {name = "title", attributes = [], children = [Text "The Sundered Grail"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Fantasy"]}), Element (TagElement {name = "price", attributes = [], children = [Text "5.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2001-09-10"]}), Element (TagElement {name = "description", attributes = [], children = [Text "The two daughters of Maeve, half-sisters, \n      battle one another for control of England. Sequel to \n      Oberon's Legacy."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk106")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Randall, Cynthia"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Lover Birds"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Romance"]}), Element (TagElement {name = "price", attributes = [], children = [Text "4.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-09-02"]}), Element (TagElement {name = "description", attributes = [], children = [Text "When Carla meets Paul at an ornithology \n      conference, tempers fly as feathers get ruffled."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk107")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Thurman, Paula"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Splish Splash"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Romance"]}), Element (TagElement {name = "price", attributes = [], children = [Text "4.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-11-02"]}), Element (TagElement {name = "description", attributes = [], children = [Text "A deep sea diver finds true love twenty \n      thousand leagues beneath the sea."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk108")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Knorr, Stefan"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Creepy Crawlies"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Horror"]}), Element (TagElement {name = "price", attributes = [], children = [Text "4.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-12-06"]}), Element (TagElement {name = "description", attributes = [], children = [Text "An anthology of horror stories about roaches,\n      centipedes, scorpions  and other insects."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk109")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Kress, Peter"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Paradox Lost"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Science Fiction"]}), Element (TagElement {name = "price", attributes = [], children = [Text "6.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-11-02"]}), Element (TagElement {name = "description", attributes = [], children = [Text "After an inadvertant trip through a Heisenberg\n      Uncertainty Device, James Salway discovers the problems \n      of being quantum."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk110")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "O'Brien, Tim"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Microsoft .NET: The Programming Bible"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Computer"]}), Element (TagElement {name = "price", attributes = [], children = [Text "36.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-12-09"]}), Element (TagElement {name = "description", attributes = [], children = [Text "Microsoft's .NET initiative is explored in \n      detail in this deep programmer's reference."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk111")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "O'Brien, Tim"]}), Element (TagElement {name = "title", attributes = [], children = [Text "MSXML3: A Comprehensive Guide"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Computer"]}), Element (TagElement {name = "price", attributes = [], children = [Text "36.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2000-12-01"]}), Element (TagElement {name = "description", attributes = [], children = [Text "The Microsoft MSXML3 parser is covered in \n      detail, with attention to XML DOM interfaces, XSLT processing, \n      SAX and more."]})]}), Element (TagElement {name = "book", attributes = [("id", "bk112")], children = [Element (TagElement {name = "author", attributes = [], children = [Text "Galos, Mike"]}), Element (TagElement {name = "title", attributes = [], children = [Text "Visual Studio 7: A Comprehensive Guide"]}), Element (TagElement {name = "genre", attributes = [], children = [Text "Computer"]}), Element (TagElement {name = "price", attributes = [], children = [Text "49.95"]}), Element (TagElement {name = "publish_date", attributes = [], children = [Text "2001-04-16"]}), Element (TagElement {name = "description", attributes = [], children = [Text "Microsoft Visual Studio 7 is explored in depth,\n      looking at how Visual Basic, Visual C++, C#, and ASP+ are \n      integrated into a comprehensive development \n      environment."]})]})]})

--json tests
test3 = do
  writeFile "testJson.json" $
    fromRight [] $
      JSON.toString $
        JsonObject [("quiz", JsonObject [("sport", JsonObject [("q1", JsonObject [("question", JsonString "Which one is correct team name in NBA?"), ("options", JsonArray [JsonString "New York Bulls", JsonString "Los Angeles Kings", JsonString "Golden State Warriros", JsonString "Huston Rocket"]), ("answer", JsonString "Huston Rocket")])]), ("maths", JsonObject [("q1", JsonObject [("question", JsonString "5 + 7 = ?"), ("options", JsonArray [JsonString "10", JsonString "11", JsonString "12", JsonString "13"]), ("answer", JsonString "12")]), ("q2", JsonObject [("question", JsonString "12 - 8 = ?"), ("options", JsonArray [JsonString "1", JsonString "2", JsonString "3", JsonString "4"]), ("answer", JsonString "4")])])])]

test4 = do
  writeFile "testJson2.json" $
    fromRight [] $
      JSON.toString $
        JsonObject [("fruit", JsonString "Apple"), ("size", JsonString "Large"), ("color", JsonString "Red")]