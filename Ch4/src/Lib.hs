module Lib
    ( greaterThan100,
    greaterThan100_2,
    greaterThan100_3,
    myTest,
    myTest',
    unschonfinkel,
    schonfinkel,
    foobar,
    foobar',
    sum',
    product',
    length',
    sum'',
    product'',
    length'',
    fun1,
    fun1',
    fun2,
    fun2',
    Tree (Leaf, Node),
    foldTree,
    insertTree,
    llRotate,
    rrRotate,
    lrRotate,
    rlRotate,
    xor,
    map',
    myFoldl
    ) where

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (> 100) xs

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

-- Currying and partial application

schonfinkel :: ((a, b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y)

unschonfinkel :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (x, y) = f x y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
    | x > 3     = (7 * x + 2) + foobar xs
    | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7 * x + 2) . filter (>3)

-- Fold

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [Integer] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold f z [] = z
fold f z (x:xs) = f x (fold f z xs)

sum'' :: [Integer] -> Integer
sum'' = fold (+) 0

product'' :: [Integer] -> Integer
product'' = fold (*) 1

length'' :: [Integer] -> Integer
length'' = fold (\_ s -> s + 1) 0

-- Exercise 1 Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f n | even n = n `div` 2
              | otherwise = 3 * n + 1

-- Exercise 2 Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = fold insertTree Leaf

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node height left value right)
    | x > value = balanceTree (Node height left value (insertTree x right))
    | x < value = balanceTree (Node height (insertTree x left) value right)
    | otherwise = Node height left value right

balanceTree :: Ord a => Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree (Node height leftTree rootValue rightTree)
    | balanceFactor tree  > 1 && (balanceFactor leftTree) >= 0 = rrRotate tree
    | balanceFactor tree  > 1 && (balanceFactor leftTree) < 0 = lrRotate tree
    | balanceFactor tree  < -1 && (balanceFactor rightTree) <= 0 = llRotate tree
    | balanceFactor tree  > 1 && (balanceFactor leftTree) > 0 = rlRotate tree
    | otherwise = Node newHeight leftTree rootValue rightTree
    where 
        tree = Node height leftTree rootValue rightTree
        newHeight = (max (h leftTree) (h rightTree)) + 1

-- Left Rotation (LL)
llRotate :: Tree a -> Tree a
llRotate (Node _ rootLeftTree rootValue (Node _ childLeftTree childValue childRightTree))
    = Node height (Node leftHeight rootLeftTree rootValue childLeftTree) childValue childRightTree
    where
        leftHeight = (max (h rootLeftTree) (h childLeftTree)) + 1 
        height = (max leftHeight (h childRightTree)) + 1

-- Right Rotation (RR)
rrRotate :: Tree a -> Tree a
rrRotate (Node _ (Node _ childLeftTree childValue childRightTree) rootValue rootRightTree)
    = Node height childLeftTree childValue (Node rightHeight childRightTree rootValue rootRightTree)
    where
        rightHeight = (max (h childRightTree) (h rootRightTree)) + 1 
        height = (max (h childLeftTree) rightHeight) + 1 

-- Left-Right Rotation (LR) or "Double left"
lrRotate :: Tree a -> Tree a
lrRotate (Node height left value right)
    = llRotate (Node height left value (rrRotate right))

-- Right-Left Rotation (RL) or "Double right"
rlRotate :: Tree a -> Tree a
rlRotate (Node height left value right)
    = rrRotate (Node height (llRotate left) value right)

h :: Tree a -> Integer
h Leaf = -1
h (Node height _ _ _) = height

balanceFactor :: Tree a -> Integer
balanceFactor Leaf = 0
balanceFactor (Node _ left _ right) = (h left) - (h right)


-- Exercise 3 More folds!
xor :: [Bool] -> Bool
xor = fold _xor False

_xor :: Bool -> Bool -> Bool
_xor a b
    | a == b = False
    | otherwise = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr f'
    where f' z x = f x z
