module Ch1
        (
        ) where

-- Ex1
-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits n 
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)] 
  
-- with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0 = []
  | otherwise = [(n `mod` 10)] ++ toDigitsRev (n `div` 10)


-- Ex2
-- We have the digits in the proper order, we need to
-- double every other one
doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:y:xs) = x: y * 2: doubleEveryOtherHelper xs
doubleEveryOtherHelper (x:[]) = x: []
 

-- Ex3
-- Calculate the sum of all digits
sumNumber :: [Integer] -> Integer
sumNumber [] = 0
sumNumber (x:xs) = x + sumNumber xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumNumber (toDigits x) + sumDigits xs

sumDigits2 :: [Integer] -> Integer
sumDigits2 l = sumNumber $ map (sumNumber . toDigits) l


-- Ex4
-- Indicate whether an Integer could be a valid credit card number
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOtherHelper $ toDigitsRev n) `mod` 10 == 0


-- Ex5
-- Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
  | n == 0 = []
  | n == 1 = [(a, c)]
  | n >= 2 = (hanoi (n - 1) a c b) ++ [(a, c)] ++ (hanoi (n - 1) b a c)
  

-- Ex6
-- Tower of Hanoi with four pegs instead of three
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]

--hanoi4 n a b c d 
--  | n == 0 = []
--  | n == 1 = [(a, d)]
--  | n >= 2 = (hanoi4 (n - 2) a c d b) ++ [(a, c), (a, d), (c, d)] ++ (hanoi4 (n - 2) b a c d)
  
--hanoi4 n a b c d
--  | n == 0 = []
--  | n == 1 = [(a, d)]
--  | n == 2 = [(a, c), (a, d), (c, d)]
--  | n >= 3 = (hanoi4 (n - 3) a c d b) ++ (hanoi 3 a c d) ++ (hanoi4 (n - 3) b a c d)


--hanoi4 n a b c d
--  | n == 0 = []
--  | n == 1 = [(a, d)]
--  | n >= 2 = (hanoi (n - 2) a d c) ++ (hanoi 2 a b d) ++ (hanoi (n - 2) c b d)



















