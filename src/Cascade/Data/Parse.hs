module Cascade.Data.Parse (Result(..), State(..), Token(..), expect, readToken) where

import Cascade.Data (Optional(..))

data Result a = Result
                { state  :: State
                , result :: a
                }
              | Error
                { message :: String }

data State = State { raw :: String }

data Token = Token
             { token_state  :: State
             , token_string :: String
             }

readToken :: State -> Int -> Token
readToken state len =
    let (State raw) = state
    in Token
       { token_state  = state { raw = (drop len raw) }
       , token_string = take len raw
       }

expect :: State -> String -> (Optional State)
expect state expected =
    let len = (length expected)
        (Token state' string) = readToken state len
    in if string == expected
        then Some state'
        else None