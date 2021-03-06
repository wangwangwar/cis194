{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch11
import Ch10
import AParser
import Data.Char
import Control.Applicative hiding (ZipList, getZipList, (*>))
import Prelude hiding (ZipList, (*>), sequenceA)
import SExpr

main = hspec $ do
  describe "Ch11" $ do
    let names = ["Joe", "Sara", "Mae"]
    let phones = ["555-5555", "123-456-7890", "555-4321"]

    describe "Applicative instance for []" $ do
      let employees1 = Employee <$> names <*> phones

      it "works like list comprehension" $ do
        length employees1 `shouldBe` 9
        head employees1 `shouldBe` Employee { name = "Joe", phone = "555-5555" }

      it "works with (.+) and (.*)" $ do
        let n = ([4, 5] .* pure 2) .+ [6, 1]
        n `shouldBe` [14, 9, 16, 11]

        let m1 = (Just 3 .+ Just 5) .* Just 8
        let m2 = (Just 3 .+ Nothing) .* Just 8

        m1 `shouldBe` Just 64
        m2 `shouldBe` Nothing

    describe "Applicative instance for ZipList" $ do

      it "works like zip" $ do
        let employee2 = getZipList $ Employee <$> ZipList names <*> ZipList phones
        employee2 `shouldBe` [Employee {name = "Joe", phone = "555-5555"},Employee {name = "Sara", phone = "123-456-7890"},Employee {name = "Mae", phone = "555-4321"}]

    describe "Applicative instance for Reader/environment" $ do
      let r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

      it "works" $ do
        let emp = getEmp r
        putStrLn $ show emp

    describe "pair" $ do

      describe "when f = Maybe" $ do

        it "returns Just their pairing if both are Just" $ do
          pair (Just 2) (Just "3") `shouldBe` Just (2, "3")

        it "returns Nothing if either of the arguments is" $ do
          pair (Just 2) Nothing `shouldBe` (Nothing :: Maybe (Int, String))
          pair Nothing (Just "3") `shouldBe` (Nothing :: Maybe (Int, String))
          pair Nothing Nothing `shouldBe` (Nothing :: Maybe (Int, String))

      describe "when f = []" $ do

        it "returns the Cartesian product of two lists" $ do
          pair [1, 2, 3] ["a", "b", "c"] `shouldBe` [(1, "a"), (1, "b"), (1, "c"),
                                                     (2, "a"), (2, "b"), (2, "c"),
                                                     (3, "a"), (3, "b"), (3, "c")]

      describe "when f = ZipList" $ do

        it "works as the same as the standard `zip` function" $ do
          pair (ZipList [1, 2, 3]) (ZipList ["a", "b", "c"]) `shouldBe` ZipList [(1, "a"), (2, "b"), (3, "c")]


    describe "(*>)" $ do

      describe "when f = Maybe" $ do

        it "returns the right part" $ do
          Just 3 *> Just "4" `shouldBe` Just "4"

      describe "when f = []" $ do

        it "returns the right part each time when" $ do
          [1, 2] *> ["a", "b", "c"] `shouldBe` ["a", "b", "c", "a", "b", "c"]

      describe "when f = ZipList" $ do

        it "returns" $ do
          ZipList [1, 2] *> ZipList ["a", "b", "c"] `shouldBe` ZipList ["a", "b"]


    describe "mapA" $ do

      describe "when f = Maybe" $ do

        it "converts map " $ do
          let func :: String -> Maybe Int = \s -> Just $ length s
          mapA func ["a", "ab", "abc"] `shouldBe` Just [1, 2, 3]

    describe "sequenceA" $ do

      describe "when f = Maybe" $ do

        it "converts [f a] -> f [a]" $ do
          sequenceA [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]
          sequenceA [Just 1, Just 2, Nothing] `shouldBe` Nothing

    describe "replicateA" $ do

      describe "when f = Maybe" $ do

        it "converts f a -> f [a]" $ do
          replicateA 3 (Just "a") `shouldBe` Just ["a", "a", "a"]

  describe "Homework" $ do

    describe "zeroOrMore" $ do

      it "parses zero or more tokens" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGH" `shouldBe` Just ("", "abcdeFGH")

    describe "oneOrMore" $ do

      it "parses one or more tokens" $ do
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGH" `shouldBe` Nothing

    describe "spaces" $ do

      it "parses zero or more spaces" $ do
        runParser spaces "ABCdEfgH" `shouldBe` Just ("", "ABCdEfgH")
        runParser spaces " ABCdEfgH" `shouldBe` Just (" ", "ABCdEfgH")

    describe "ident" $ do

      it "parses an identifier" $ do
        runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
        runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
        runParser ident "2bad" `shouldBe` Nothing
        runParser ident "" `shouldBe` Nothing

    describe "parseSExpr" $ do

      it "parses S-expressions 5" $ do
        runParser parseSExpr "5" `shouldBe` Just (A $ N 5, "")

      it "parses S-expressions foo3" $ do
        runParser parseSExpr "foo3" `shouldBe` Just (A $ I "foo3", "")

      it "parses S-expressions (bar (foo) 3 5 874)" $ do
        runParser parseSExpr "(bar (foo) 3 5 874)" `shouldNotBe` Nothing

      it "parses S-expressions (((lambda x (lambda y (plus x y))) 3) 5)" $ do
        runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldNotBe` Nothing

      it "parses S-expressions (    lots   of    (   spaces   in  )  this ( one ) )" $ do
        runParser parseSExpr "(    lots   of    (   spaces   in  )  this ( one ) )" `shouldNotBe` Nothing

      it "parses S-expressions \"3abc\" is valid" $ do
        runParser parseSExpr "3abc" `shouldBe` Just (A $ N 3, "abc")

    --  it "parses S-expressions \"(3abc)\" is valid" $ do
    --    runParser parseSExpr "(3abc)" `shouldBe` Just (A $ N 3, "abc")

      it "parses S-expressions \"(abc 3\" is invalid" $ do
        runParser parseSExpr "(abc 3" `shouldBe` Nothing

      it "parses S-expressions \"\" is invalid" $ do
        runParser parseSExpr "" `shouldBe` Nothing
