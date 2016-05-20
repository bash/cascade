module Cascade.Parse.Whitespace (parseWhitespace, parseNewline) where

import Cascade.Data.Ast (Item(Whitespace))
import Cascade.Data.Parse (Result(..), State, Token(..), expectOne)

newlines = ["\r\n", "\n", "\r", "\f"]
whitespace = [" ", "\t"] ++ newlines

parseNewline :: State -> (Result Item)
parseNewline state =
    parse' state newlines

parseWhitespace :: State -> (Result Item)
parseWhitespace state =
    parse' state whitespace

parse' :: State -> [String] -> (Result Item)
parse' state expected =
    unwrap' (expectOne state expected)

unwrap' :: (Maybe Token) -> (Result Item)
unwrap' (Just (Token state' string)) = Result { state = state', result = (Whitespace string) }
unwrap' (Nothing) = Error { message = "expected a whitespace" }