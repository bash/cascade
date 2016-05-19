module Cascade.Data (Item(..), Optional(..)) where

data Item = Comment
            { content :: String
            }
            | AtCharsetRule
            | Whitespace String

    deriving (Eq, Read)

instance Show Item where
    show (Comment content) = "/*" ++ content ++ "*/"
    show (AtCharsetRule) = "@charset;"
    show (Whitespace whitespace) = whitespace

data Optional a = Some a | None
