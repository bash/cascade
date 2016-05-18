module Cascade.Data (Item(..)) where

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
