module JoinList
    (
        JoinList (..),
        (+++)
    ) where

-- Monoidally Annotated Join-Lists

-- data JoinListBasic a = Empty
--                      | Single a
--                      | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty             = []
-- jlbToList (Single a)        = [a]
-- jlbToList (Append l1 l2)    = jlbToList l1 ++ jlbToList l2


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
