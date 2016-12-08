module Calc
    ( eval,
      evalStr,
      lit,
      add,
      mul,
      MinMax (MinMax),
      Mod7 (Mod7),
      testInteger,
      testBool,
      testMM,
      testSat
    ) where

import ExprT
import Parser

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)

-- Exercise 3

-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit x = Mod7 $ mod x 7
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
