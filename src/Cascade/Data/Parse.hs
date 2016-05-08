module Cascade.Data.Parse (Result(..), State(..), expect) where

data Result a = Result
                { state  :: State
                , result :: a
                }
              | Error
                { message :: String }

data State = State { raw :: String }

expect :: State -> String -> Bool
expect (State raw) expected =
    let len = (length expected)
    in (take len raw) == expected