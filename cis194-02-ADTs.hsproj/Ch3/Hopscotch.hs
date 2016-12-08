module Ch3.Hopscotch where

skipsInner :: Int -> [a] -> ([a], [a])
skipsInner _ [] = ([], [])
skipsInner 0 list = ([], list)
skipsInner 1 [x:xs] = ([x], xs)
skipsInner n [x:xs] = skipsInner (n - 1) xs

skipsInner2 :: Int -> [a] -> [a]
skipsInner2 n list 
    let 
        (result, tail) = skipsInner n list
    in
        | tail == [] = result
        | otherwise = result + skipsInner n tail
  
--skips :: [a] -> [[a]]
