{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch9
import AParser
import Data.Char


main = hspec $ do
  describe "Ch9" $ do
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


    describe "AParser" $ do

      it "implement a `Functor` instance for `Parser`" $ do
        runParser (isDigit <$> satisfy isUpper) "ABC" `shouldBe` Just (False, "BC")
        runParser (isDigit <$> satisfy isUpper) "abc" `shouldBe` Nothing
        runParser (isAlpha <$> satisfy (== 'x')) "xyz" `shouldBe` Just (True, "yz")

      it "implement a `Applicative` instance for `Parser`" $ do
        let parseN :: Parser Char = satisfy (== 'N')
        let parseA :: Parser Char = satisfy (== 'A')
        let parseM :: Parser Char = satisfy (== 'M')
        let parseE :: Parser Char = satisfy (== 'E')

        let parseName :: Parser Name = (\x y z a -> x:(y:(z:[a]))) <$> parseN <*> parseA <*> parseM <*> parseE
        runParser parseName "NAME" `shouldBe` Just ("NAME", "")

        let parsePhone :: Parser Integer = posInt
        let parseEmployee :: Parser Employee = Employee <$> parseName <*> (show `fmap` parsePhone)
        runParser parseEmployee "Name1234a" `shouldBe` Nothing
        runParser parseEmployee "NAME1234a" `shouldBe` Just (Employee { name = "NAME", phone = "1234" }, "a")
