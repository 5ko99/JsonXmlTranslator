import Data.JsonObject
import Data.XMLObject
import qualified Parsers.JsonParser as JSONParser
import qualified Parsers.XMLParser as XMLParser
import qualified Stringify.Json as JSON
import qualified Stringify.Xml as XML
import Test.Hspec
import qualified Translation.ToJson as ToJSON
import qualified Translation.ToXml as ToXML

main :: IO ()
main = do
  print $ XMLParser.parse "<a>2</a>"
  print $ JSONParser.parse "{\"a\":2}"
