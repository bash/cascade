module Main where

import Cascade.Parse (parseComment, parseAtCharset, parseWhitespace, doParse)
import Cascade.Data.Parse (Result(..), State(..))
import Cascade.Data.Ast (Item(..))

showItemsList :: [Item] -> String
showItemsList [] = ""
showItemsList (x:xs) =
    (show x) ++ (showItemsList xs)

mainParse :: String -> IO ()
mainParse raw =
    let result = doParse [parseAtCharset, parseComment, parseWhitespace] State { raw = raw }
    in case result of
        Result _ items -> putStrLn (showItemsList items)
        Error message -> putStrLn ("Oops: " ++ message)

main :: IO ()
main =
    do
        source <- readFile "data/test.css"
        mainParse source
