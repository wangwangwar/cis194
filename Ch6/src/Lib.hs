-- http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf

module Lib
    ( knapsack01,
    example,
    fib,
    fibs1,
    fibs2,
    Stream (Empty, Stream),
    streamToList,
    streamRepeat,
    streamMap,
    streamFromSeed,
    nats,
    ruler
    ) where

import Data.Array

knapsack01 :: [Double]  -- values
    -> [Integer]        -- nonnegative weights
    -> Integer          -- knapsack size
    -> Double           -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
    where 
        numItems = length vs
        m = array ((-1, 0), (numItems-1, maxW)) $
            [((-1, w), 0) | w <- [0..maxW]] ++
            [((i, 0), 0) | i <- [0..numItems-1]] ++
            [((i, w), best) | i <- [0..numItems-1], w <- [1..maxW], let best | ws!!i > w = m!(i-1, w) | otherwise = max (m!(i-1, w)) (m!(i-1, w-ws!!i) + vs!!i)]

example = knapsack01 [3, 4, 5, 8, 10] [2, 3, 4, 5, 9] 20


-- Exercise 1

-- fib n computes the nth Fibonacci number Fn
fib :: Integer -> Integer
fib n = fibInner n 0 1

fibInner :: Integer -> Integer -> Integer -> Integer
fibInner 0 a b = a
fibInner n a b = fibInner (n - 1) b (a + b)

-- The infinite list of all Fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [1..] 

-- Exercise 2

-- More efficient implementation for fibs
fibs2 :: [Integer]
fibs2 = fibs2Inner 1 1

fibs2Inner :: Integer -> Integer -> [Integer]
fibs2Inner a b = a: (fibs2Inner b (a + b))

-- Exercise 3

data Stream a = Empty
              | Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList Empty = []
streamToList (Stream x stream) = x: (streamToList stream)

instance Show a => Show (Stream a) where
    show stream = unwords $ map show $ take 20 $ streamToList stream

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap _ Empty = Empty
streamMap fun (Stream x stream) = Stream (fun x) (streamMap fun stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed transform seed = Stream seed (streamFromSeed transform (transform seed))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- interleave streams [[0, 0, ...], [1, 1, ...], [2, 2, ...], ...]
ruler :: Stream Integer
ruler = interleaveStreams (streamMap streamRepeat nats)

-- interleave two streams
interleaveTwoStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveTwoStreams (Stream x stream1) stream2 = Stream x (interleaveTwoStreams stream2 stream1)

-- interleave the Stream of Stream of Integer
interleaveStreams :: Stream (Stream Integer) -> Stream Integer
interleaveStreams (Stream xs restStream) = interleaveTwoStreams xs (interleaveStreams restStream)
