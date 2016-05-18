module Cascade.Data.Parse (Result(..), State(..), expect) where

import Cascade.Data (Optional(..))

data Result a = Result
                { state  :: State
                , result :: a
                }
              | Error
                { message :: String }

data State = State { raw :: String }

expect' :: State -> String -> Bool
expect' (State raw) expected =
    let len = (length expected)
    in (take len raw) == expected

expect :: State -> String -> (Optional State)
expect state expected =
    let len = (length expected)
        (State raw) = state
    in if (expect' state expected)
        then Some state { raw = (drop len raw) }
        else None