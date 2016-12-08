module Lib
    ( greaterThan100,
    greaterThan100_2,
    greaterThan100_3,
    myTest,
    myTest',
    unschonfinkel,
    schonfinkel
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

schonfinkel :: ((a, b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y)

unschonfinkel :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (x, y) = f x y
