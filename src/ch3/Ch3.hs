module Ch3 where
  
-- Recursion patterns

-- Map

data IntList = Empty | Cons Int IntList
  deriving Show
  
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

type Operation = Int -> Int

mapIntList :: Operation -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList operation (Cons x xs) = Cons (operation x) (mapIntList operation xs)


-- Filter

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs
  

-- Polymorphism

data List t = E | C t (List t) deriving Show

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs
