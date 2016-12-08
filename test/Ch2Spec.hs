-- Haskell Playground 1.0


let shoe = Shoe

let ex01 = Failure
let ex02 = OK 3.4

safeDiv 0 1
safeDiv 1 0

let brent = Person "Brent" 31 SealingWax
let stan = Person "Stan" 94 Cabbage


getAge brent

baz brent

checkFav brent
checkFav stan

ex03

intListProd (Cons 3 (Cons 2 (Cons 1 Empty)))

tree


import Ch2.LogAnalysis

parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
parseMessage "This is not in the right format" == Unknown "This is not in the right format"

let message1 = parseMessage "E 2 562 help help" 
let message2 = parseMessage "I 29 la la la"
let message3 = parseMessage "This is not in the right format"

testParse parse 100 "error.log"

let tree = insert message1 Leaf
let tree2 = insert message2 tree
let tree3 = insert message3 tree2

tree3

let messages = [message1, message2, message3]
let t = build messages


inOrder t

testWhatWentWrong parse whatWentWrong "sample.log"
