module Cascade.Parse.Comment (parseComment) where

import Cascade.Data.Parse (State(..), Result(..), Token(Token), expect, readToken)
import Cascade.Data.Ast (Item(Comment))

commentStart = "/*"
commentEnd = "*/"

parseComment :: State -> (Result Item)
parseComment state =
    case (expect state commentStart) of
        (Just state') -> (make' (parse'  state' ""))
        (Nothing) -> Error { message = "expected /*" }

parse' :: State -> String -> (Result String)
parse' state body =
    case (expect state commentEnd) of
        (Just state') -> Result { state = state', result = body }
        (Nothing) ->
            let (Token state' string) = readToken state 1
            in parse' state' (body ++ string)

make' :: (Result String) -> (Result Item)
make' (Error message) = Error { message = message }
make' (Result state result) =
    Result { state = state, result = Comment result }