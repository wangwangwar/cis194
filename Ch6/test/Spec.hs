import Test.HUnit
import System.IO
import Lib

tests = TestList
    [ "example" ~: example ~?= 26.0,
    "fib 0" ~: fib 0 ~?= 0,
    "fib 1" ~: fib 1 ~?= 1,
    "fib 7" ~: fib 7 ~?= 13,
    "take 5 fibs1" ~: take 5 fibs1 ~?= [1, 1, 2, 3, 5],
    "take 5 fibs2" ~: take 5 fibs2 ~?= [1, 1, 2, 3, 5]
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
