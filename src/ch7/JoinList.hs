module JoinList
    (
        JoinList (..),
        (+++),
        indexJ,
        (!!?),
        jlToList,
        dropJ,
        takeJ,
        Score (..),
        score,
        scoreString,
        scoreLine
    ) where

import Sized
import Data.Monoid

-- Monoidally Annotated Join-Lists


data JoinList m a = JEmpty
                  | JSingle m a
                  | JAppend m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Append (Product 210)
--   (Append (Product 30)
--     (Single (Product 5) ’y’)
--     (Append (Product 6)
--     (Single (Product 2) ’e’)
--     (Single (Product 3) ’a’)))
--     (Single (Product 7) ’h’)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag JEmpty = mempty
tag (JSingle m _) = m
tag (JAppend m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = JAppend (tag j1 `mappend` tag j2) j1 j2

-- Exercise 2

-- Helper
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList JEmpty = []
jlToList (JSingle _ a) = [a]
jlToList (JAppend _ l1 l2) = jlToList l1 ++ jlToList l2

len :: (Sized b, Monoid b) => JoinList b a -> Int
len x = getSize (size (tag x))

-- indexJ finds the JoinList element at the specified index
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ JEmpty = Nothing
indexJ 0 (JSingle m a) = Just a
indexJ _ (JSingle _ _) = Nothing
indexJ i (JAppend m left right) 
    | i >= rootSize = Nothing 
    | i < leftSize = indexJ i left
    | otherwise = indexJ (i - leftSize) right
    where 
        rootSize =  getSize (size m)
        leftSize = getSize (size (tag left))
        rightSize = getSize (size (tag right))

-- dropJ drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n xs
    | n <= 0 = xs
dropJ n xs@(JAppend _ left right)
    | n < (len left) = dropJ n left +++ right
    | otherwise = dropJ (n - (len left)) right
dropJ _ _ = JEmpty

-- takeJ returns the first n elements of a JoinList, dropping all other elements
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n xs
    | n >= (len xs) = xs
takeJ n xs@(JAppend _ left right)
    | n > (len left) = left +++ takeJ (n - (len left)) right
    | otherwise = takeJ n left
takeJ _ _ = JEmpty

-- Exercise 3

data Score = Score Int
    deriving (Eq, Show)

instance Monoid Score where
    mempty = Score 0
    mappend = \(Score x) (Score y) -> Score (x + y)

score :: Char -> Score
score c
    | c `elem` "esirantolu" = Score 1
    | c `elem` "dg" = Score 2
    | c `elem` "cpmb" = Score 3
    | c `elem` "hyfvw" = Score 4
    | c `elem` "k" = Score 5
    | c `elem` "xj" = Score 8
    | c `elem` "zq" = Score 10
    | otherwise = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine s = JSingle (scoreString s) s

-- Exercise 4

instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    mappend (a1, b1) (a2, b2) = (mappend a1 a2, mappend b1 b2)
