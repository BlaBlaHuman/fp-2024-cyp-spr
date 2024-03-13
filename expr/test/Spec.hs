import Test.Tasty
import Test.Tasty.HUnit
import Expr
import Eval
import Error
import Simplify
import qualified Data.Map.Strict as M

testEval :: TestTree
testEval =
    testGroup "Eval" [ evalCases ]
  where
    testEval msg expr res =
      testCase msg $ eval expr M.empty @?= Right res
    testEvalErr msg expr err =
      testCase msg $ eval expr M.empty @?= Left err
    testEvalEnv msg expr env res =
      testCase msg $ eval expr env @?= Right res
    testEvalEnvErr msg expr env err =
      testCase msg $ eval expr env @?= Left err
    evalCases = testGroup "Eval"
      [
        testEval "Id" 10 10,
        testEval "Id" (-5) (-5),
        testEval "Square root" (Root 16) 4,
        testEvalErr "Negative root" (Root $ -9) NegRoot,
        testEval "Plus" (BinOp 5 3 Plus) 8,
        testEval "Minus" (BinOp 7 2 Minus) 5,
        testEval "Mul" (BinOp 2 4 Mul) 8,
        testEval "Div" (BinOp 10 2 Div) 5,
        testEvalErr "Division by zero" (BinOp 10 0 Div) ZeroDiv,
        testEval "Pow" (BinOp 2 3 Pow) 8,
        testEvalErr "Square root via pow" (BinOp (-2) (Number 0.5) Pow) NegRoot,
        testEval "Complex" (BinOp (Root 64) 2 Plus) 10,
        testEvalErr "Complex negative root" (BinOp (Root $ -9) 3 Mul) NegRoot,
        testEvalErr "Complex division by zero" (BinOp (BinOp 5 0 Div) 3 Mul) ZeroDiv,
        testEval "Complex" (BinOp (BinOp 4 3 Mul) 2 Div) 6,
        testEvalEnv "Defined var" (Variable "x") (M.fromList [("x", 10)]) 10,
        testEvalErr "Undefined var" (Variable "x") (UndefinedVariable "x"),
        testEvalEnv "Binop var" (BinOp (Variable "x") 2 Plus) (M.fromList [("x", 10)]) 12,
        testEvalErr "Binop ondufined var" (BinOp (Variable "x") 2 Plus) (UndefinedVariable "x"),
        testEvalEnvErr "Division by zero var" (BinOp 2 (Variable "x") Div) (M.fromList [("x", 0)]) ZeroDiv,
        testEvalEnv "Several vars" (BinOp (Variable "x") (Variable "x") Plus) (M.fromList [("x", 10)]) 20,
        testEvalEnv "Several vars" (BinOp (Variable "x") (Variable "y") Mul) (M.fromList [("x", 10), ("y", 2)]) 20
      ]

testSimplify :: TestTree
testSimplify =
      testGroup "Simplify" [ simplifyCases ]
  where
    testSimplify msg expr res =
      testCase msg $ simplify expr @?= res

    simplifyCases = testGroup "Simplify"
      [
        testSimplify "1/2 Pow is root" (BinOp 5 (Number 0.5) Pow) (Root 5),
        testSimplify "Mul 0 right" (BinOp 5 0 Mul) 0,
        testSimplify "Mul 0 left" (BinOp 0 5 Mul) 0,
        testSimplify "Mul 1 right" (BinOp (BinOp 8 7 Plus) 1 Mul) (BinOp 8 7 Plus),
        testSimplify "Mul 1 left" (BinOp 1 (BinOp 8 7 Plus) Mul) (BinOp 8 7 Plus),
        testSimplify "Plus 0 right" (BinOp (BinOp 8 7 Plus) 0 Plus) (BinOp 8 7 Plus),
        testSimplify "Plus 0 left" (BinOp 0 (BinOp 8 7 Plus) Plus) (BinOp 8 7 Plus),
        testSimplify "x - x" (BinOp (Variable "x") (Variable "x") Minus) 0,
        testSimplify "Simplify binop" (BinOp (BinOp 0 5 Mul) (BinOp (BinOp 8 7 Plus) 0 Plus) Minus) (BinOp 0 (BinOp 8 7 Plus) Minus),
        testSimplify "Zero root" (Root (BinOp 0 5 Mul)) 0,
        testSimplify "1 * 1" (Root (BinOp 1 1 Mul)) 1,
        testSimplify "Var id" (Variable "x") (Variable "x"),
        testSimplify "Num id" 10 10
      ]

main :: IO ()
main = do
    defaultMain $ testGroup "Expressions" [ testEval, testSimplify ]
