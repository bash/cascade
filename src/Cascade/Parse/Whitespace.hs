module Cascade.Parse.Whitespace (parseWhitespace) where

import Cascade.Data.Ast (Item(Whitespace))
import Cascade.Data.Parse (Result(..), State, Token(..), expectOne)

whitespace = [" ", "\t", "\n"]

parseWhitespace :: State -> (Result Item)
parseWhitespace state =
    unwrap' (expectOne state whitespace)

unwrap' :: (Maybe Token) -> (Result Item)
unwrap' (Just (Token state' string)) = Result { state = state', result = (Whitespace string) }
unwrap' (Nothing) = Error { message = "expected a whitespace" }