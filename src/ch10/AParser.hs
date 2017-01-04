{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser
    (
    Parser (..),
    satisfy,
    char,
    posInt,
    abParser,
    abParser_,
    intPair,
    intOrUppercase
    ) where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1

-- First, you’ll need to implement a Functor instance for Parser.
-- Hint: You may find it useful to implement a function
-- first :: (a -> b) -> (a,c) -> (b,c)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser a) = Parser g
    where
      g s = case runParser (Parser a) s of
        Nothing -> Nothing
        Just (x, xs) -> Just $ first f (x, xs)

-- Exercise 2

-- implement an Applicative instance for Parser
--
-- 1. pure a represents the parser which consumes no input and successfully
-- returns a result of a.
--
-- 2. p1 <*> p2 represents the parser which first runs p1 (which will
-- consume some input and produce a function), then passes the
-- remaining input to p2 (which consumes more input and produces
-- some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should
-- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
-- p2 succeed).
instance Applicative Parser where
  pure a = Parser f
    where
      f s = Just (a, s)

  p1 <*> p2 = Parser f
    where
      f s = case runParser p1 s of
        Nothing -> Nothing
        Just (x, xs) -> case runParser p2 xs of
          Nothing -> Nothing
          Just (y, ys) -> Just (x y, ys)

-- Exercise 3

-- We can also test your Applicative instance using other simple
-- applications of functions to multiple parsers. You should implement
-- each of the following exercises using the Applicative interface to put
-- together simpler parsers into more complex ones. Do not implement
-- them using the low-level definition of a Parser! In other words, pretend
-- that you do not have access to the Parser constructor or even
-- know how the Parser type is defined.
abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> satisfy (== 'a') <*> satisfy (== 'b')

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> satisfy (== ' ') <*> posInt

-- Exercise 4

-- Applicative by itself can be used to make parsers for simple, fixed
-- formats. But for any format involving choice (e.g. “. . . after the colon
-- there can be a number or a word or parentheses. . . ”) Applicative is
-- not quite enough. To handle choice we turn to the Alternative class,
-- defined (essentially) as follows:
-- ```
-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a
-- ```
-- (<|>) is intended to represent choice: that is, f1 <|> f2 represents
-- a choice between f1 and f2. empty should be the identity element for
-- (<|>), and often represents failure.
instance Alternative Parser where
  -- empty represents the parser which always fails.
  empty = Parser f where f s = Nothing
  -- • p1 <|> p2 represents the parser which first tries running p1. If
  -- p1 succeeds then p2 is ignored and the result of p1 is returned.
  -- Otherwise, if p1 fails, then p2 is tried instead.
  p1 <|> p2 = Parser f
      where
        f s = case runParser p1 s of
          Nothing -> runParser p2 s
          Just x -> Just x

-- Exercise 5

--  parses either an integer value or an uppercase character, and fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
