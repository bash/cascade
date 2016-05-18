module Cascade.Parse (parseComment, parseAt, tryParse, doParse) where

import Cascade.Data (Item(..))
import Cascade.Data.Parse (Result(..), State(..), expect)
import Cascade.Data (Optional(..))

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

-- Todo: remove this function
parseAt :: State -> (Result Item)
parseAt state =
    case (expect state "@") of
        (Some state') -> Result { state = state', result = At { at = "@" } }
        (None) -> Error { message = "did not found expected @" }

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
            let (State raw) = state
                newRaw = (drop 1 raw)
                newBody = (body ++ (take 1 raw))
            in parseCommentUntil state { raw = newRaw } newBody
