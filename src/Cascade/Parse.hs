module Cascade.Parse (parseComment, parseAt, tryParse) where

import Cascade.Data (Item(..))
import Cascade.Data.Parse (Result(..), State(..), expect)

type Parser = (State -> Result Item)
type Parsers = [Parser]

tryParse :: Parsers -> State -> Result Item
tryParse [] state = Error { message = "no suitable parser found" }
tryParse (parser:parsers) state =
    let result = parser state
    in case result of
        (Error _) -> tryParse parsers state
        (Result _ _) -> result



parseComment :: State -> (Result Item)
parseComment state =
    let (State raw) = state
    in if not (expect state "/*")
        then Error { message = "comment has to start with a /*" }
        else createComment (parseCommentUntil state { raw = (drop 2 raw) } "")

-- Todo: remove this function
parseAt :: State -> (Result Item)
parseAt state =
    let (State raw) = state
    in if not (expect state "@")
        then Error { message = "did not found expected @" }
        else Result { state = state { raw = (drop 2 raw) }, result = At { at = "@" } }

createComment :: (Result String) -> (Result Item)
createComment (Result state result) = Result
                                      { state  = state
                                      , result = Comment { content = result }
                                      }

createComment (Error message) = Error { message = message }

parseCommentUntil :: State -> String -> (Result String)
parseCommentUntil state body =
    let (State raw) = state
    in if expect state "*/"
        then Result
            { state  = state
            , result = body
            }
        else let newRaw = (drop 1 raw)
                 newBody = (body ++ (take 1 raw))
        in parseCommentUntil state { raw = newRaw } newBody
