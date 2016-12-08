import Test.HUnit
import System.IO
import Lib

tests = TestList
    [ "example" ~: example ~?= 26.0,
    "fib 0" ~: fib 0 ~?= 0,
    "fib 1" ~: fib 1 ~?= 1,
    "fib 7" ~: fib 7 ~?= 13,
    "take 5 fibs1" ~: take 5 fibs1 ~?= [1, 1, 2, 3, 5],
    "take 5 fibs2" ~: take 5 fibs2 ~?= [1, 1, 2, 3, 5],
    "streamToList (Stream 3 Empty)" ~: streamToList (Stream 3 Empty) ~?= [3],
    "show streamRepeat 3" ~: show (streamRepeat 3) ~?= unwords (replicate 20 "3"),
    "show (streamMap (+1) (streamRepeat 3))" ~: show (streamMap (+1) (streamRepeat 3)) ~?= unwords (replicate 20 "4"),
    "take 5 (streamToList (streamFromSeed (+1) 3))" ~: take 5 (streamToList (streamFromSeed (+1) 3)) ~?= [3, 4, 5, 6, 7],
    "nats" ~: take 5 (streamToList nats) ~?= [0, 1, 2, 3, 4],
    "ruler" ~: take 8 (streamToList ruler) ~?= [0, 1, 0, 2, 0, 1, 0, 3]
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
