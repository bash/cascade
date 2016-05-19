module Cascade.Parse.AtCharsetRule (parseAtCharsetRule) where

import Cascade.Data.Ast (Item(AtCharsetRule))
import Cascade.Data.Parse (State, Result(..), expect)

-- Todo: implement parsing the actual charset
parseAtCharsetRule :: State -> (Result Item)
parseAtCharsetRule state =
    case (expect state "@charset;") of
        (Just state') -> Result { state = state', result = AtCharsetRule }
        (Nothing) -> Error { message = "was expecting @charset;" }
