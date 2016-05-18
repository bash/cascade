module Cascade.Data (Item(..), Optional(..)) where

data Item = Comment
            { content :: String
            }
            | At
            { at :: String
            }

    deriving (Eq, Read)

instance Show Item where
    show (Comment content) = "/*" ++ content ++ "*/"
    show (At _) = "@"

data Optional a = Some a | None
