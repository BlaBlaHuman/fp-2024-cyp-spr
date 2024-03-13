module Error ( Error(..) ) where

data Error =
    ZeroDiv
    | NegRoot
    | UndefinedVariable String
    deriving Eq

instance Show Error where
    show ZeroDiv = "A division by zero occured"
    show NegRoot = "A square root of a negative number was taken"
    show (UndefinedVariable name) = "The variable " ++ name ++ " is not defined"