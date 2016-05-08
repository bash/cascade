module Main where

import Cascade.Parse (parseComment)
import Cascade.Data.Parse (Result(..), State(..))

main :: IO ()
main =
    let result = parseComment State { raw = "/* foo bar baz */" }
    in case result of
        Result _ comment -> putStrLn (show comment)
        Error message -> putStrLn("Oops: " ++ message)