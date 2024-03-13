module Expr ( Operation(..), Expr(..) ) where

data Operation =
    Plus
    | Minus
    | Mul
    | Div
    | Pow
    deriving (Show, Eq)

data Expr a =
    Number a
    | Root (Expr a)
    | BinOp (Expr a) (Expr a) Operation
    | Variable String
    deriving Eq

instance Show a => Show (Expr a) where
    show (Number x) = show x
    show (Root x) = "√(" ++ show x ++ ")"
    show (BinOp e1 e2 op) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Variable name) = name


instance Num a => Num (Expr a) where
    (+) e1 e2 = BinOp e1 e2 Plus
    (-) e1 e2 = BinOp e1 e2 Minus
    (*) e1 e2 = BinOp e1 e2 Mul
    negate e = BinOp (Number 0) e Minus
    abs = undefined
    signum = undefined
    fromInteger = Number . fromInteger