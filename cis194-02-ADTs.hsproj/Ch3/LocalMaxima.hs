module Ch3.LocalMaxima where

localMaxima :: [Integer] -> [Integer]
localMaxima [x0, x1] = []
localMaxima (x0:x1:x2:xs)
    | x0 <= x1 && x1 >= x2 = x1:localMaxima (x1:x2:xs)
    | otherwise = localMaxima (x1:x2:xs)

_ = localMaxima [2, 9, 5, 6, 1] == [9, 6]
_ = localMaxima [2, 3, 4, 1, 5] == [4]
_ = localMaxima [1, 2, 3, 4, 5] == []
