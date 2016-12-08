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
    "length'' [1, 2, 3, 4, 5]" ~: length'' [1, 2, 3, 4, 5] ~?= 5,
    "fun1 [1, 2, 3, 4, 5]" ~: fun1 [1, 2, 3, 4, 5] ~?= 0,
    "fun1 [1, 3, 4, 5]" ~: fun1 [1, 3, 4, 5] ~?= 2,
    "fun1' [1, 2, 3, 4, 5]" ~: fun1' [1, 2, 3, 4, 5] ~?= 0,
    "fun1' [1, 3, 4, 5]" ~: fun1' [1, 3, 4, 5] ~?= 2,
    "fun2 1" ~: fun2 1 ~?= 0,
    "fun2 2" ~: fun2 2 ~?= 2,
    "fun2 3" ~: fun2 3 ~?= 40,
    "fun2' 1" ~: fun2' 1 ~?= 0,
    "fun2' 2" ~: fun2' 2 ~?= 2,
    "fun2' 3" ~: fun2' 3 ~?= 40,
    "rrRotate (Node 2 (Node 1 (Node 0 Leaf 1 Leaf) 2 (Node 0 Leaf 3 Leaf)) 4 (Node 0 Leaf 5 Leaf))" ~: rrRotate (Node 2 (Node 1 (Node 0 Leaf 1 Leaf) 2 (Node 0 Leaf 3 Leaf)) 4 (Node 0 Leaf 5 Leaf)) ~?= Node 2 (Node 0 Leaf 1 Leaf) 2 (Node 1 (Node 0 Leaf 3 Leaf) 4 (Node 0 Leaf 5 Leaf)),
    "insertTree 3 Leaf" ~: insertTree 3 Leaf ~?= Node 0 Leaf 3 Leaf,
    "insertTree 3 (Node 0 Leaf 1 Leaf)" ~: insertTree 3 (Node 0 Leaf 1 Leaf) ~?= Node 1 Leaf 1 (Node 0 Leaf 3 Leaf),
    "foldTree [12]'" ~: foldTree [1, 2] ~?= Node 1 (Node 0 Leaf 1 Leaf) 2 Leaf,
    "foldTree 'ABCDEFGHIJ'" ~: foldTree "ABCDEFGHIJ" ~?= Node 3 (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'B' Leaf) 'C' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' (Node 0 Leaf 'F' Leaf))) 'G' (Node 1 (Node 0 Leaf 'H' Leaf) 'I' (Node 0 Leaf 'J' Leaf)),
    "xor [False, True, False]" ~: xor [False, True, False] ~?= True,
    "xor [False, True, False, False, True]" ~: xor [False, True, False, False, True] ~?= False,
    "xor []" ~: xor [] ~?= False,
    "map' (+1) [1, 2, 3]" ~: map' (+1) [1, 2, 3] ~?= [2, 3, 4],
    "myFoldl (+) 1 [1, 2, 3]" ~: myFoldl (+) 1 [1, 2, 3] ~?= 7,
    "take 5 (foldr (:) [] [1..])" ~: take 5 (foldr (:) [] [1..]) ~?= [1, 2, 3, 4, 5],
    "take 5 (myFoldl (\\z x -> (x:z)) [] [1..])" ~: take 5 (myFoldl (\z x -> (x:z)) [] [1..]) ~?= [1, 2, 3, 4, 5]
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
