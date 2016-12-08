import Test.HUnit
import System.IO
import Lib
import Calc
import ExprT
import Parser

tests = TestList
    [ "toList True" ~: toList True ~?= [1],
    "toList [1, 2, 3]" ~: toList ([1, 2, 3] :: [Int]) ~?= [1, 2, 3],
    "sumL 3" ~: sumL (3 :: Int) ~?= 3,
    "sumL [1, 2, 3]" ~: sumL ([1, 2, 3] :: [Int]) ~?= 6,
    "eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))" ~: eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ~?= 20,
    "parseExp Lit Add Mul \"(2+3)*4\"" ~: parseExp Lit Add Mul "(2+3)*4" ~?= Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4)),
    "evalStr \"(2+3)*4\"" ~: evalStr "(2+3)*4" ~?= Just 20,
    "mul (add (lit 2) (lit 3)) (lit 4) :: ExprT" ~: (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) ~?= Mul (Add (Lit 2) (Lit 3)) (Lit 4),
    "mul (add (lit 0) (lit 1)) (lit 2) :: Bool" ~: (mul (add (lit 0) (lit 1)) (lit 2) :: Bool) ~?= True,
    "mul (add (lit 2) (lit 3)) (lit 4) :: MinMax" ~: mul (add (lit (2 :: Integer)) (lit (3 :: Integer))) (lit (4 :: Integer)) ~?= MinMax 3,
    "mul (add (lit 2) (lit 3)) (lit 4) :: Mod7" ~: (mul (add (lit 2) (lit 3)) (lit 4)) ~?= Mod7 6 
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
