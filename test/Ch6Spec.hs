import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch6


main = hspec $ do
    describe "Knapsack exmaple" $ do
        it "returns 26" $ do
            knapsackExample `shouldBe` 26.0
    
    describe "fib" $ do
        it "returns 0th Fibonacci number 0" $ do
            fib 0 `shouldBe` 0
        
        it "returns 1st Fibonacci number 1" $ do
            fib 1 `shouldBe` 1

        it "returns 7th Fibonacci number 13" $ do
            fib 7 `shouldBe` 13

    describe "fibs1" $ do
        it "returns a inifite list of all Fibonacci numbers" $ do
            take 5 fibs1 `shouldBe` [1, 1, 2, 3, 5]

    describe "fibs2" $ do
        it "returns a inifite list of all Fibonacci numbers" $ do
            take 5 fibs2 `shouldBe` [1, 1, 2, 3, 5]

    describe "streamToList" $ do
        it "returns a list of the stream" $ do
            streamToList (Stream 1 (Stream 2 (Stream 3 Empty))) `shouldBe` [1, 2, 3]

    describe "streamRepeat" $ do
        it "returns a stream containing infinitely many copies of the given element" $ do
            show (streamRepeat 3) `shouldBe` unwords (replicate 20 "3")

    describe "streamMap" $ do
        it "returns a stream applying a function to every element of a Stream" $ do
            show (streamMap (+1) (streamRepeat 3)) `shouldBe` unwords (replicate 20 "4")

    describe "streamFromSeed" $ do
        it "generates a Stream from a “seed”" $ do
            take 5 (streamToList (streamFromSeed (+1) 3)) `shouldBe` [3, 4, 5, 6, 7]

    describe "nats" $ do
        it "generates a Stream from the infinite list of natural numbers 0, 1, 2, . . ." $ do
            take 5 (streamToList nats) `shouldBe` [0, 1, 2, 3, 4]

    describe "ruler" $ do
        it "generates a Stream from the infinite list of natural numbers 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . ." $ do
            take 8 (streamToList ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3]

