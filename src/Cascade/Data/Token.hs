module Cascade.Data.Token (readToken, Token(..)) where

import Cascade.Data.Parse (State(..))

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