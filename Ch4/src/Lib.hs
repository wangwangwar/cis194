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
    length''
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
