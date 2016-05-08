module Main where

import Cascade.Parse (parseComment)
import Cascade.Data.Parse (Result(..), State(..), getCurrentInput)

main :: IO ()
main =
    let result = parseComment State { input = "/* foo bar baz */", position = 0 }
    in case result of
        Result _ comment -> putStrLn (show comment)
        Error message -> putStrLn("Oops: " ++ message)