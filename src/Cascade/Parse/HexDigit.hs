module Cascade.Parse.HexDigit (parseHexDigit) where

import Cascade.Data.Ast (Item(HexDigit))
import Cascade.Data.Parse (Result(..), State, Token(..), expectOne)

numbers = map show [0..9]
hexDigits = numbers

parseHexDigit :: State -> (Result Item)
parseHexDigit state =
    let (Result state' string) = parse' state ""
    in Result { result = HexDigit string, state = state' }

parse' :: State -> String -> (Result String)
parse' state string =
    case (expectOne state hexDigits) of
        Nothing -> Result { state = state, result = string }
        (Just (Token state' string')) -> parse' state' (string ++ string')
