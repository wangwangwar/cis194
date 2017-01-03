{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf

module Ch11
    (
    (.+),
    (.*),
    ZipList (..),
    BigRecord (..),
    getEmp,
    pair,
    (*>),
    mapA,
    sequenceA,
    replicateA
    ) where

import Prelude hiding (ZipList, (*>), sequenceA)
import Control.Applicative hiding (ZipList, (*>))
import Ch10

-- class (Functor f) => Applicative f where
--     pure  :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative [] where
--   pure a = [a]
--   [] <*> _ = []
--   (f:fs) <*> as = (map f as) ++ (fs <*> as)

(.+) :: (Applicative f, Num a) => f a -> f a -> f a
(.+) = liftA2 (+)

(.*) :: (Applicative f, Num a) => f a -> f a -> f a
(.*) = liftA2 (*)


newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


-- Reader / Environment
-- instance Functor ((->) e) where
--   fmap = (.)
--
-- instance Applicative ((->) e) where
--   pure = const
--   f <*> x = \e -> (f e) (x e)

data BigRecord = BR { getName         :: Name
                    , getSSN          :: String
                    , getSalary       :: Integer
                    , getPhone        :: String
                    , getLicensePlate :: String
                    , getNumSickDays  :: Int
                    }

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

-- One of the benefits of having a unified interface like Applicative is that we
-- can write generic tools and control structures that work with any type which
-- is an instance of Applicative.
pair :: Applicative f => f a -> f b -> f (a, b)
-- pair fa fb = (\x y -> (x, y)) <$> fa <*> fb
-- pair fa fb = (,) <$> fa <*> fb
-- pair fa fb = liftA2 (,) fa fb
pair = liftA2 (,)

-- Can you implement the following functions? Consider what each function does
-- when f is replaced with each of the above types.
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (\_ x -> x)

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA afb = foldr ((\fx fxs -> (:) <$> fx <*> fxs) . afb) (pure [])

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (\fx fxs -> (:) <$> fx <*> fxs) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA i fa = replicate i <$> fa
