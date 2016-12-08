import Test.HUnit
import System.IO
import Lib

tests = TestList
    [ "greaterThan100 [1, 50, 100, 150]" ~: greaterThan100 [1, 50, 100, 150] ~?= [150],
    "greaterThan100_2 [1, 50, 100, 150]" ~: greaterThan100_2 [1, 50, 100, 150] ~?= [150],
    "greaterThan100_3 [1, 50, 100, 150]" ~: greaterThan100_3 [1, 50, 100, 150] ~?= [150],
    "myTest [1, 50, 100, 150, 200]" ~: myTest [1, 50, 100, 150, 200] ~?= True,
    "myTest' [1, 50, 100, 150, 200]" ~: myTest' [1, 50, 100, 150, 200] ~?= True,
    "foobar [0, 1, 2, 3, 4, 5]" ~: foobar [0, 1, 2, 3, 4, 5] ~?= 67,
    "foobar' [0, 1, 2, 3, 4, 5]" ~: foobar' [0, 1, 2, 3, 4, 5] ~?= 67,
    "sum' [1, 2, 3, 4, 5]" ~: sum' [1, 2, 3, 4, 5] ~?= 15,
    "product' [1, 2, 3, 4, 5]" ~: product' [1, 2, 3, 4, 5] ~?= 120,
    "length' [1, 2, 3, 4, 5]" ~: length' [1, 2, 3, 4, 5] ~?= 5,
    "sum'' [1, 2, 3, 4, 5]" ~: sum'' [1, 2, 3, 4, 5] ~?= 15,
    "product'' [1, 2, 3, 4, 5]" ~: product'' [1, 2, 3, 4, 5] ~?= 120,
    "length'' [1, 2, 3, 4, 5]" ~: length'' [1, 2, 3, 4, 5] ~?= 5
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
