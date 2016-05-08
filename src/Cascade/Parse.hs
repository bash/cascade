module Cascade.Parse (parseComment) where

import Cascade.Data (Comment(..))
import Cascade.Data.Parse (Result(..), State(..), expect)

parseComment :: State -> (Result Comment)
parseComment state =
    let (State raw) = state
    in if not (expect state "/*")
        then Error { message = "comment has to start with a /*" }
        else createComment (parseCommentUntil state { raw = (drop 2 raw) } "")

createComment :: (Result String) -> (Result Comment)
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
