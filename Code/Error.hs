module Error where

-- data type for errors in this scripting language
data Error = ParseError String -- Use for problems with Parser
            | MathError String -- Use for incorrect mathematics
            | ValueError String -- Use for incorrect data types
            | TreeError String -- Use with BinaryTree.hs
    deriving (Eq)

instance Show Error where
    show (ParseError i) = "ParseError: " ++ i 
    show (MathError f) =  "MathError: " ++ f
    show (ValueError s) = "ValueError: " ++ s
    show (TreeError t) = "TreeError: " ++ t