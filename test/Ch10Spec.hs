{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch10
import AParser
import Data.Char
import Control.Applicative


main = hspec $ do
  describe "Ch10" $ do
    describe "<*>" $ do
      let m_name1 :: Maybe Name = Nothing
      let m_name2 :: Maybe Name = Just "Brent"

      let m_phone1 :: Maybe String = Nothing
      let m_phone2 :: Maybe String = Just "555-1234"

      it "works correctly" $ do
        Employee <$> m_name1 <*> m_phone1 `shouldBe` Nothing
        Employee <$> m_name1 <*> m_phone2 `shouldBe` Nothing
        Employee <$> m_name2 <*> m_phone1 `shouldBe` Nothing
        Employee <$> m_name2 <*> m_phone2 `shouldBe` Just Employee { name = "Brent", phone = "555-1234" }


    describe "Parser" $ do

      it "implement a `Functor` instance for `Parser`" $ do
        runParser (isDigit <$> satisfy isUpper) "ABC" `shouldBe` Just (False, "BC")
        runParser (isDigit <$> satisfy isUpper) "abc" `shouldBe` Nothing
        runParser (isAlpha <$> satisfy (== 'x')) "xyz" `shouldBe` Just (True, "yz")

      it "implement a `Applicative` instance for `Parser`" $ do
        let parseN :: Parser Char = satisfy (== 'N')
        let parseA :: Parser Char = satisfy (== 'A')
        let parseM :: Parser Char = satisfy (== 'M')
        let parseE :: Parser Char = satisfy (== 'E')

        let parseName :: Parser Name = (\x y z a -> [x, y, z, a]) <$> parseN <*> parseA <*> parseM <*> parseE
        runParser parseName "NAME" `shouldBe` Just ("NAME", "")

        let parsePhone :: Parser Integer = posInt
        let parseEmployee :: Parser Employee = Employee <$> parseName <*> (show `fmap` parsePhone)
        runParser parseEmployee "Name1234a" `shouldBe` Nothing
        runParser parseEmployee "NAME1234a" `shouldBe` Just (Employee { name = "NAME", phone = "1234" }, "a")

      describe "abParser" $ do
        it "expects to see the characters ’a’ and ’b’ and returns them as a pair." $ do
          runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
          runParser abParser "aecdef" `shouldBe` Nothing

      describe "abParser_" $ do
        it "acts in the same way as abParser but returns () instead of the characters ’a’ and ’b’." $ do
          runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
          runParser abParser_ "aecdef" `shouldBe` Nothing

      describe "initPair" $ do
        it "reads two integer values separated by a space and returns the integer values in a list. " $ do
          runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

      describe "`Alternative` instance for `Parser`" $ do
        it "works correctly" $ do
          let aOrbParser = satisfy (== 'a') <|> satisfy (== 'b')
          runParser aOrbParser "axy" `shouldBe` Just ('a', "xy")
          runParser aOrbParser "bxy" `shouldBe` Just ('b', "xy")
          runParser aOrbParser "xy" `shouldBe` Nothing

      describe "intOrUppercase" $ do
        it " parses either an integer value or an uppercase character, and fails otherwise." $ do
          runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
          runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
          runParser intOrUppercase "foo" `shouldBe` Nothing
