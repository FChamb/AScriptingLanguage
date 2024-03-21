module Error where

data Error = ParseError String | MathError String | ValueError String
    deriving (Eq)

instance Show Error where
    show (ParseError i) = "ParseError: " ++ i
    show (MathError f) =  "MathError: " ++ f
    show (ValueError s) = "ValueError: " ++ s