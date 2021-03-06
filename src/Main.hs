module Main where

import Cascade.Data.Parse (Result(..), State(..))
import Cascade.Data.Ast (Item(..))

import Cascade.Parse (doParse)
import Cascade.Parse.Comment (parseComment)
import Cascade.Parse.Whitespace (parseWhitespace)
import Cascade.Parse.AtCharsetRule (parseAtCharsetRule)
import Cascade.Parse.HexDigit (parseHexDigit)

showItemsList :: [Item] -> String
showItemsList [] = ""
showItemsList (x:xs) =
    (show x) ++ (showItemsList xs)

mainParse :: String -> IO ()
mainParse raw =
    let result = doParse [parseAtCharsetRule, parseComment, parseWhitespace, parseHexDigit] State { raw = raw }
    in case result of
        Result _ items -> putStrLn (showItemsList items)
        Error message -> putStrLn ("Oops: " ++ message)

main :: IO ()
main =
    do
        source <- readFile "data/test.css"
        mainParse source
