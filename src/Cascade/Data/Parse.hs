module Cascade.Data.Parse (Result(..), State(..), getCurrentInput, expect) where

data Result a = Result
                { state  :: State
                , result :: a
                }
              | Error
                { message :: String }

data State = State
    { input    :: String
    , position :: Int
    }

getCurrentInput :: State -> String
getCurrentInput (State input position) =
    drop position input

expect :: State -> String -> Bool
expect state expected =
    (take (length expected) (getCurrentInput state)) == expected