module Cascade.Parse (parseComment, parseAtCharset, parseWhitespace, tryParse, doParse) where

import Cascade.Data.Ast (Item(..), Optional(..))
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


parseComment :: State -> (Result Item)
parseComment state =
    case (expect state "/*") of
        (Some state') -> (createComment (parseCommentUntil state' ""))
        (None) -> Error { message = "comment has to start with a /*" }

-- Todo: implement parsing the actual charset
parseAtCharset :: State -> (Result Item)
parseAtCharset state =
    case (expect state "@charset;") of
        (Some state') -> Result { state = state', result = AtCharsetRule }
        (None) -> Error { message = "was expecting @charset;" }

parseWhitespace :: State -> (Result Item)
parseWhitespace state =
    case (expectOne state [" ", "\t", "\n"]) of
        (Some (Token state' string)) -> Result { state = state', result = (Whitespace string) }
        (None) -> Error { message = "expected a whitespace" }

createComment :: (Result String) -> (Result Item)
createComment (Result state result) = Result
                                      { state  = state
                                      , result = Comment { content = result }
                                      }

createComment (Error message) = Error { message = message }

parseCommentUntil :: State -> String -> (Result String)
parseCommentUntil state body =
    case (expect state "*/") of
        (Some state') -> Result { state = state', result = body }
        (None) ->
            let (Token state' string) = readToken state 1
            in parseCommentUntil state' (body ++ string)