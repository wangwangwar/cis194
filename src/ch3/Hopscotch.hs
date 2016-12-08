module Ch3.Hopscotch where

-- Get a tuple of n-th element and the rest list after the n-th element.
skipsNth :: Int -> [a] -> ([a], [a])
skipsNth _ [] = ([], [])
skipsNth 0 list = ([], list)
skipsNth 1 (x:xs) = ([x], xs)
skipsNth n (x:xs) = skipsNth (n - 1) xs

-- Skips every n-th elements.
skipsEveryNth :: Int -> [a] -> [a]
skipsEveryNth n list 
    | n == 0 = list
    | null tail = result
    | otherwise = result ++ skipsEveryNth n tail
    where
        (result, tail) = skipsNth n list
  
-- The output of skips is a list of lists. The first list in the output
-- should be the same as the input list. The second list in the output
-- should contain every second element from the input list...and the nth
-- list in the output should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips list = 
    skipsInner 1 list []
    where 
        skipsInner n list acc
            | n <= 0 = []
            | n > length list = acc
            | otherwise = skipsInner (n + 1) list (acc ++ [(skipsEveryNth n list)])
