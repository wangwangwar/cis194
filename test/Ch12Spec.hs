import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch12

main = hspec $ do

  describe "Ch12 Examples" $ do

    describe "`check` and `halve`" $ do
      let ex01 = return 7 >>= check >>= halve
      let ex02 = return 12 >>= check >>= halve
      let ex03 = return 12 >>= halve >>= check

      it "works in sequence" $ do
        ex01 `shouldBe` Nothing
        ex02 `shouldBe` Nothing
        ex03 `shouldBe` Just 6

    describe "`addOneOrTwo`" $ do
      let ex04 = [10, 20, 30] >>= addOneOrTwo

      it "works" $ do
        ex04 `shouldBe` [11, 12, 21, 22, 31, 32]

    describe "`parseFile`" $ do

      it "works" $ do
        runParser parseFile "4 78 19 3 44 3 1 7 5 2 3 2" `shouldBe` Just ([[78, 19, 3, 44], [1, 7, 5], [3, 2]], "")

  describe "Ch12 Homeworks" $ do

    describe "" $ do
      it "" $ do
        True `shouldBe` False
