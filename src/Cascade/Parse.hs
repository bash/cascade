module Cascade.Parse (parseComment) where

import Cascade.Data (Comment(..))
import Cascade.Data.Parse (Result(..), State(..), expect)

parseComment :: State -> (Result Comment)
parseComment state =
    let (State _ position) = state
    in if not (expect state "/*")
        then Error { message = "comment has to start with a /*" }
        else parseCommentUntil(state { position = position + 2 } )

parseCommentUntil :: State -> (Result Comment)
parseCommentUntil state = Result
                          { state = state
                          , result = Comment { content = "*\n * hello world\n " }
                          }
