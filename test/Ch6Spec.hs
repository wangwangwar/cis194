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

    describe "Num (Stream Integer)" $ do
        it "returns Stream Integer with fromInteger function" $ do
            take 5 (streamToList ((fromInteger 3) :: Stream Integer)) `shouldBe` [3, 0, 0, 0, 0]

        it "returns a Stream of negated coefficients when negate" $ do
            take 5 (streamToList (negate (streamFromSeed (+1) 0 :: Stream Integer))) `shouldBe` [0, -1, -2, -3, -4]

        it "returns the sum of two Streams with (+)" $ do
            take 5 (streamToList ((streamFromSeed (+1) 0 :: Stream Integer) + (streamFromSeed (+2) 0) :: Stream Integer)) `shouldBe` [0, 3, 6, 9, 12]

        it "returns the multiplication of two: 6x^3 + 7x^2 - 10x + 9 * -2x^3  + 4x - 5 = -12x^6 - 14x^5 + 44x^4 - 20x^3 - 75x^2 + 86x - 45 with (*)" $ do
            streamToList ((Stream 9 (Stream (-10) (Stream 7 (Stream 6 Empty))) * Stream (-5) (Stream 4 (Stream 0 (Stream (-2) Empty)))) :: Stream Integer) `shouldBe` [-45, 86, -75, -20, 44, -14, -12]

        it "returns the division of two: (x^3 - 12x^2 - 42) / (x - 3) = x^2 - 9x - 27 with (/)" $ do
            take 10 (streamToList ((Stream (-42) (Stream 0 (Stream (-12) (Stream 1 Empty)))) / (Stream (-3) (Stream 1 Empty)) :: Stream Integer)) `shouldBe` [-27, -9, 1]

        it "fibs3" $ do
            take 10 (streamToList fibs3) `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

        it "Matrix2 * Matrix2" $ do
            (Matrix2 1 1 1 0 :: Matrix2 Integer) * (Matrix2 1 1 1 0 :: Matrix2 Integer) `shouldBe` (Matrix2 2 1 1 1 :: Matrix2 Integer)

        it "fib4 10 == 55" $ do
            fib4 10 `shouldBe` 55
