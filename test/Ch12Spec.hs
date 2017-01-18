import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch12
import Risk

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

    describe "die" $ do
      it "returns random integer between 1 and 6" $ do
        g <- getStdGen
        let r = evalRand die g
        ([1..6] :: [Int]) `shouldContain` [unDV r]

    describe "dies" $ do
      it "returns a Rand to produce a list of DieValue" $ do
        g <- getStdGen
        let rs = evalRand (dies 3) g
        length rs `shouldBe` 3
        all (\r -> unDV r <= 6 || unDV r >= 1) rs `shouldBe` True

    describe "instance Monoid Battlefield" $ do
      it "works correctly" $ do
        let b1 = Battlefield { attackers = 1, defenders = 2 }
        let b2 = Battlefield { attackers = 3, defenders = 4 }
        b1 <> b2 `shouldBe` Battlefield { attackers = 4, defenders = 6 }

    describe "battleResults" $ do
      it "computes the battle results" $ do
        battleResults [2 :: DieValue, 3 :: DieValue] [4 :: DieValue, 1 :: DieValue]
          `shouldBe` Battlefield { attackers = -1, defenders = -1 }
        battleResults [2 :: DieValue, 3 :: DieValue] [4 :: DieValue]
          `shouldBe` Battlefield { attackers = -1, defenders = 0 }

    describe "battle" $ do
      it "works correctly" $ do
        let battlefield = Battlefield { attackers = 4, defenders = 4 }
        g <- getStdGen
        let b = evalRand (battle battlefield) g
        print b

    describe "invade" $ do
      it "works correctly" $ do
        let battlefield = Battlefield { attackers = 6, defenders = 4 }
        g <- getStdGen
        let b = evalRand (invade battlefield) g
        print b
        attackers b < 2 || defenders b <= 0 `shouldBe` True

    describe "successProb" $ do
      it "computes the success probability" $ do
        let battlefield = Battlefield { attackers = 20, defenders = 20 }
        g <- getStdGen
        let b = evalRand (successProb battlefield) g
        print b
