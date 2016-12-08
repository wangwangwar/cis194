import Test.HUnit
import System.IO
import Lib

tests = TestList
    [ "greaterThan100 [1, 50, 100, 150]" ~: greaterThan100 [1, 50, 100, 150] ~?= [150],
    "greaterThan100_2 [1, 50, 100, 150]" ~: greaterThan100_2 [1, 50, 100, 150] ~?= [150],
    "greaterThan100_3 [1, 50, 100, 150]" ~: greaterThan100_3 [1, 50, 100, 150] ~?= [150],
    "myTest [1, 50, 100, 150, 200]" ~: myTest [1, 50, 100, 150, 200] ~?= True,
    "myTest' [1, 50, 100, 150, 200]" ~: myTest' [1, 50, 100, 150, 200] ~?= True
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
