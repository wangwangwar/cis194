
-- http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf

module Ch10
    (
    Name,
    Employee (..)
    ) where


type Name = String

data Employee = Employee {
  name :: Name,
  phone :: String
} deriving (Show, Eq)

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   _ <*> Nothing = Nothing
--   Just f <*> Just x = Just (f x)
