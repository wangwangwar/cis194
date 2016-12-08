import Test.HUnit
import System.IO
import Ch3


let exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

let addOne x = x + 1
let square x = x * x

mapIntList addOne exampleList
mapIntList square exampleList

keepOnlyEven exampleList


filterList even lst1

