module Cascade.Parse (parseAtCharset, parseWhitespace, tryParse, doParse) where

import Cascade.Data.Ast (Item(..))
import Cascade.Data.Parse (Result(..), State(..), Token(..), expect, expectOne, readToken)

type Parser = (State -> Result Item)
type Parsers = [Parser]


tryParse :: Parsers -> State -> Result Item
tryParse [] _ = Error { message = ("no suitable parser found") }
tryParse (parser:parsers) state =
    let result = parser state
    in case result of
        (Error _) -> tryParse parsers state
        (Result _ _) -> result

doParse :: Parsers -> State -> Result [Item]
doParse parsers state = doParse' parsers state []

doParse' :: Parsers -> State -> [Item] -> Result [Item]
doParse' parsers state results =
    let (State raw) = state
    in if raw == ""
        then Result { state = state, result = results }
        else
            let result = tryParse parsers state
            in case result of
                (Error message) -> Error { message = message }
                (Result state' inner) -> doParse' parsers state' (results ++ [inner])

-- Todo: implement parsing the actual charset
parseAtCharset :: State -> (Result Item)
parseAtCharset state =
    case (expect state "@charset;") of
        (Just state') -> Result { state = state', result = AtCharsetRule }
        (Nothing) -> Error { message = "was expecting @charset;" }

parseWhitespace :: State -> (Result Item)
parseWhitespace state =
    case (expectOne state [" ", "\t", "\n"]) of
        (Just (Token state' string)) -> Result { state = state', result = (Whitespace string) }
        (Nothing) -> Error { message = "expected a whitespace" }