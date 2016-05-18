module Main where

import Cascade.Parse (parseComment, parseAt, doParse)
import Cascade.Data.Parse (Result(..), State(..))

main :: IO ()
main =
    let result = doParse [parseAt, parseComment] State { raw = "/* foo bar baz */@@@" }
    in case result of
        Result _ comment -> putStrLn (show comment)
        Error message -> putStrLn("Oops: " ++ message)