-- http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf

module Ch12
    (
    check,
    halve,
    addOneOrTwo,
    parseFile,
    runParser,
    ) where

import AParser
import Control.Applicative
import SExpr

-- instance Monad Maybe where
--   return = Just
--   Nothing >>= _ = Nothing
--   Just x >>= k = k x

-- Examples
check :: Int -> Maybe Int
check n | n < 10 = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n = Just $ n `div` 2
        | otherwise = Nothing

-- instance Monad [] where
--   return x = [x]
--   xs >>= k = concat (map k xs)

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

-- Monad combinators

-- First, sequence takes a list of monadic values and produces a single monadic
-- value which collects the results. What this means depends on the particular
-- monad.
--
-- sequence :: Monad m => [m a] -> m [a]
-- sequence [] = return []
-- sequence (ma:mas) =
--   ma >>= \a -> sequence mas >>= \as -> return a:as

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

instance Monad Parser where
  return = pure
  p >>= q = Parser $ \s -> case runParser p s of
    Nothing -> Nothing
    Just (x, xs) -> runParser (q x) xs

parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt

parseInt :: Parser Int
parseInt = fromInteger <$> spacesWith posInt
