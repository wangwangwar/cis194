module Main where

import System.Environment
import Ch8
import Employee
import Data.Tree

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      putStrLn $ formatTree testCompany
    else do
      fileContent <- readFile $ head args
      let tree = read fileContent :: Tree Employee
      putStrLn $ formatTree tree
