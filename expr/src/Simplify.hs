module Simplify ( simplify ) where

import Expr

simplify :: RealFloat a => Expr a -> Expr a
simplify (BinOp 0 _ Mul) = Number 0
simplify (BinOp _ 0 Mul) = Number 0
simplify (BinOp 1 x Mul) = x
simplify (BinOp x 1 Mul) = x
simplify (BinOp x 0 Plus) = x
simplify (BinOp 0 x Plus) = x
simplify (BinOp e (Number 0.5) Pow) = Root $ simplify e
simplify (BinOp x y Minus)
    | x == y = Number 0
simplify (BinOp x y op) = BinOp (simplify x) (simplify y) op
simplify (Root x)
    | simplify x == 0 = 0
    | simplify x == 1 = 1
simplify (Root x) = Root $ simplify x
simplify x = x