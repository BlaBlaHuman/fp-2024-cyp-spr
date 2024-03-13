module Eval ( eval ) where

import Expr
import Data.Map.Strict as M
import Error

eval :: RealFloat a =>  Expr a -> Map String a -> Either Error a
eval (Number x) _ = Right x
eval (Root x) env =
    case eval x env of
    Right result -> if result >= 0 then Right $ sqrt result else Left NegRoot
    Left err -> Left err
eval (BinOp x y op) env =
    case eval x env of
    Right result1 -> case eval y env of
        Right result2 -> applyOp op result1 result2
        Left err -> Left err
    Left err -> Left err
eval (Variable name) vars =
    case M.lookup name vars of
    Just x -> Right x
    Nothing -> Left (UndefinedVariable name)


applyOp :: RealFloat a => Operation -> a -> a -> Either Error a
applyOp Div x y
    | y == 0 = Left ZeroDiv
    | otherwise = Right (x / y)
applyOp Pow x y
  | isNaN (x ** y) = Left NegRoot
  | otherwise = Right (x ** y)
applyOp op x y = Right (opToFun op x y)
  where opToFun :: RealFloat a => Operation -> a -> a -> a
        opToFun Plus = (+)
        opToFun Minus = (-)
        opToFun Mul = (*)