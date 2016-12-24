
-- http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf

module Ch8
    (
    glCons,
    moreFun,
    treeFold
    ) where

import Employee
import Data.Tree
import Data.Monoid

-- Exercise 1

-- Adds an Employee to the GuestList (updating the cached Fun score appropriately)
--
-- Of course, in general this is impossible:
-- the updated fun score should depend on whether the Employee
-- being added is already in the list, or if any of their direct subordinates
-- are in the list, and so on. For our purposes, though, you
-- may assume that none of these special cases will hold: that is,
-- glCons should simply add the new Employee and add their fun
-- score without doing any kind of checks.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl fun) = GL (gl ++ [e]) (fun + empFun e)

-- A Monoid instance for GuestList

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

-- Takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score. (If the scores are equal it
-- does not matter which is returned.)
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) = if fun1 > fun2 then gl1 else gl2


-- Exercise 2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold func b t = func (rootLabel t) (treeListFold func b (subForest t))

treeListFold :: (a -> b -> b) -> b -> [Tree a] -> b
treeListFold func b [] = b
treeListFold func b (t:ts) = treeListFold func (treeFold func b t) ts
