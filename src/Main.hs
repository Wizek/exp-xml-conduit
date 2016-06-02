-- module Main where

-- main :: IO ()
-- main = do
--   putStrLn "hello world"

-- {-# LANGUAGE OverloadedStrings #-}
-- import Control.Monad.Trans.Resource
-- import Data.Conduit (($$))
-- import Data.Text (Text, unpack)
-- import Text.XML.Stream.Parse
-- import Control.Applicative ((<*))

-- data Person = Person Int Text
--         deriving Show

-- -- Do I need to change the lambda function \age to something else to get both name and age?
-- parsePerson = tagNoAttr "person" $ \age -> do
--         name <- content  -- How do I get age from the content?  "unpack" is for attributes
--         return $ Person age name

-- parsePeople = tagNoAttr "people" $ many parsePerson

-- -- This doesn't ignore the xmlns attributes
-- parsePopulation  = tagName "population" (optionalAttr "xmlns" <* ignoreAttrs) $ parsePeople

-- main = do
--         people <- runResourceT $
--              parseFile def "imp.xml" $$ parsePopulation
--         print people


{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Trans.Resource (runResourceT)
-- import           Control.Monad.Except
import           Control.Monad.Catch          (MonadThrow)
import           Data.Conduit                 (Consumer, ($$))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy                    as L
import qualified Data.Text.Lazy.IO                    as L
import           Data.Text.Read               (decimal)
import           Data.XML.Types               (Event)
import           Text.XML.Stream.Parse
import           System.IO
import           System.IO
import           Text.Hamlet.XML
import           Text.XML
import           Text.Show.Pretty
import qualified Data.Map as Map
import           ComposeLTR


data Person = Person Int Text
        deriving Show

-- Do I need to change the lambda function \age to something else to get both name and age?
parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson = tagNoAttr "{http://example.com}person" $ do
        name <- force "firstname tag missing" $ tagNoAttr "{http://example.com}firstname" content
        ageText <- force "age tag missing" $ tagNoAttr "{http://example.com}age" content
        case decimal (T.strip ageText) of
            Right (age, "") -> return $ Person age (T.strip name)
            Left a -> force ("invalid age value: " ++ a) $ return Nothing

parsePeople :: MonadThrow m => Consumer Event m [Person]
parsePeople = force "no people tag" $ do
    _ <- tagNoAttr "{http://example.com}success" content
    _ <- tagNoAttr "{http://example.com}row_count" content
    _ <- tagNoAttr "{http://example.com}summary" $
        tagNoAttr "{http://example.com}bananas" content
    tagNoAttr "{http://example.com}people" $ many parsePerson

-- This doesn't ignore the xmlns attributes
parsePopulation :: MonadThrow m => Consumer Event m [Person]
parsePopulation = force "population tag missing" $
    tagName "{http://example.com}population" ignoreAttrs $ \() -> parsePeople

-- fName = "/home/wizek/sandbox/exp-xml/exp-xml-conduit/inp.xml"
fName = "/home/wizek/sandbox/exp-xml/exp-xml-conduit/inpLarge2.xml"
-- fName = "inp.xml"

-- instance PrettyVal Node
-- instance GHC.Generics.Generic Node

xmlNodes = [xml|
  $forall a <- xs
    ^{xmlNode "a" a}
|]
  where
    xs = [0..100000]
  -- #{xmlNode "b" 2}

xmlNode name age = [xml|
  <person>
    <firstname>#{name}
    <age>#{T.pack $ show age}
|]
  -- <person>
  --     <firstname>Eliezer</firstname>
  --     <age>2</age>
  -- </person>

-- documentify nodes = Document (Prologue [] Nothing []) (Element "root" [] nodes) []
documentify nodes = Document (Prologue [] Nothing []) (Element "root" Map.empty nodes) []

main :: IO ()
main = do
  hPutStrLn stderr "init"
  people <- runResourceT $
    parseFile def fName $$ parsePopulation
  print $ take 2 $ drop 100 people
  -- print xmlNodes
  -- putStrLn $ dumpStr xmlNodes
  -- L.putStrLn $ renderText def{rsPretty=True} $ documentify xmlNodes
