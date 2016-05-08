module Cascade.Data (Comment(..)) where

data Comment = Comment
    { content :: String } deriving (Eq, Read)

instance Show Comment where
    show (Comment content) = "/*" ++ content ++ "*/"

