{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf

module Ch6
    (
    knapsackExample,
    fib,
    fibs1,
    fibs2,
    Stream (Empty, Stream),
    streamToList,
    streamRepeat,
    streamMap,
    streamFromSeed,
    nats,
    ruler,
    fibs3
    ) where

import Data.Array
import Debug.Trace

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

knapsackExample = knapsack01 [3, 4, 5, 8, 10] [2, 3, 4, 5, 9] 20


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
fibs2Inner a b = a: fibs2Inner b (a + b)

-- Exercise 3

data Stream a = Empty
              | Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList Empty = []
streamToList (Stream x stream) = x: streamToList stream

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

-- Exercise 6 (Optional)
--
-- The essential idea is to work with generating functions of the form
-- a0 + a1x + a2x2 + · · · + anxn + . . .
-- where x is just a “formal parameter” (that is, we will never actually
-- substitute any values for x; we just use it as a placeholder) and all the
-- coefficients ai are integers. We will store the coefficients a0, a1, a2, . . .
-- in a Stream Integer.

-- x = 0 + 1x + 0x^2 + 0x^3 + ...
-- x :: Stream Integer

instance Num (Stream Integer) where
    -- n = n + 0x + 0x^2 + 0x^3 + ...
    fromInteger n = Stream n (streamRepeat 0)
    -- to negate a generating function, negate all its coefficients
    negate = streamMap negate
    -- (a0 + a1x + a2x^2 + . . .) + (b0 + b1x + b2x^2 + . . .) = 
    -- (a0 + b0) + (a1 + b1)x + (a2 + b2)x^2 + . . .
    (+) (Stream x stream1) (Stream y stream2) = Stream (x + y) (stream1 + stream2)
    (+) Empty stream2 = stream2
    (+) stream1 Empty = stream1
    -- Suppose A = a0 + xA' and B = b0 + xB'
    -- AB = (a0 + xA')B
    --    = a0B + xA'B
    --    = a0(b0 + xB') + xA'B
    --    = a0b0 + x(a0B' + A'B)
    (*) (Stream a0 a') (Stream b0 b') = Stream (a0 * b0) (streamMap (* a0) b' + (a' * Stream b0 b'))
    (*) Empty _ = Empty
    (*) _ Empty = Empty

-- Suppose A = a0 + xA' and B = b0 + xB'
-- A/B = (a0 / b0) + x((1 / b0)(A' - QB'))
instance Fractional (Stream Integer) where
    (Stream a0 a') / (Stream b0 b') = q where
        tr x0 = floor (fromIntegral x0 / fromIntegral b0 :: Double) 
        hed = floor (fromIntegral a0 / fromIntegral b0 :: Double)
        q = Stream hed (streamMap tr (a' - (q * b')))

-- F(x) = x / (1 - x - x^2)
fibs3 :: Stream Integer
fibs3 = Stream 0 (Stream 1 Empty) / Stream 1 (Stream (-1) (Stream (-1) Empty))
