{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy.IO            as TL
import           Prelude hiding (readFile)
import           System.IO hiding (readFile)
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Map as Map
import           ComposeLTR


data Person = Person Int Text
        deriving Show

-- fName = "/home/wizek/sandbox/exp-xml/exp-xml-conduit/inp.xml"
fName = "/home/wizek/sandbox/exp-xml/exp-xml-conduit/inpLarge2.xml"

ppl cur = cur $/ element "people" &/ element "person" $> map person

person cur = Person age name
  where
  name = cur $/ element "firstname" &/ content $> T.concat $> T.strip
  age = cur $/ element "age" &/ content $> T.concat $> T.unpack $> read

main :: IO ()
main = do
  hPutStrLn stderr "init"
  doc <- readFile def fName

  print (doc $> fromDocument $> ppl $> drop 1000 $> take 4)


