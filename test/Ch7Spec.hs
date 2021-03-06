import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch7
import JoinList
import Sized
import Data.Monoid


main = hspec $ do

    describe "`leaf` construct a leaf of the `Tree`" $ do

        it "returns the leaf with element x" $ do
            leaf 4 `shouldBe` Node Empty 4 Empty

    describe "`treeSize` returns the size of the Tree" $ do

        it "returns 0 for the size of the `Empty` tree" $ do
            treeSize Empty `shouldBe` 0

        it "returns 1 for the size of the leaf" $ do
            treeSize (leaf 4) `shouldBe` 1

        it "returns 3 for the size of the `Tree` ((3) 4 (5))" $ do
            treeSize (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) `shouldBe` 3

    describe "`treeSum` returns the sum of the value of `Node`s of the entire Tree" $ do

        it "returns 0 for the `Empty` tree" $ do
            treeSum Empty `shouldBe` 0

        it "returns 3 for the leaf 3" $ do
            treeSum (leaf 3) `shouldBe` 3

        it "returns 12 for the tree ((3) 4 (5))" $ do
            treeSum (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) `shouldBe` 12

    describe "`treeDepth` returns the depth of the Tree" $ do

        it "returns 0 for the `Empty` tree" $ do
            treeDepth Empty `shouldBe` 0

        it "returns 1 for the leaf" $ do
            treeDepth (leaf 3) `shouldBe` 1

        it "returns 3 for the tree (((3) 4 (5)) 6 (7))" $ do
            treeDepth (Node (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) 6 (Node Empty 7 Empty)) `shouldBe` 3

    describe "`flatten` returns the list of the `Node` in inorder sequence" $ do

        it "returns [] for the `Empty` tree" $ do
            flatten Empty `shouldBe` ([] :: [Integer])

        it "returns [3] for the leaf 3" $ do
            flatten (leaf 3) `shouldBe` [3]

        it "returns [3 4 5] for the tree ((3) 4 (5))" $ do
            flatten (Node (leaf 3) 4 (leaf 5)) `shouldBe` [3, 4, 5]

    describe "`treeSize'` returns the size of the Tree" $ do

        it "returns 0 for the size of the `Empty` tree" $ do
            treeSize' Empty `shouldBe` 0

        it "returns 1 for the size of the leaf" $ do
            treeSize' (leaf 4) `shouldBe` 1

        it "returns 3 for the size of the `Tree` ((3) 4 (5))" $ do
            treeSize' (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) `shouldBe` 3

    describe "`treeSum'` returns the sum of the value of `Node`s of the entire Tree" $ do

        it "returns 0 for the `Empty` tree" $ do
            treeSum' Empty `shouldBe` 0

        it "returns 3 for the leaf 3" $ do
            treeSum' (leaf 3) `shouldBe` 3

        it "returns 12 for the tree ((3) 4 (5))" $ do
            treeSum' (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) `shouldBe` 12

    describe "`treeDepth'` returns the depth of the Tree" $ do

        it "returns 0 for the `Empty` tree" $ do
            treeDepth' Empty `shouldBe` 0

        it "returns 1 for the leaf" $ do
            treeDepth' (leaf 3) `shouldBe` 1

        it "returns 3 for the tree (((3) 4 (5)) 6 (7))" $ do
            treeDepth' (Node (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)) 6 (Node Empty 7 Empty)) `shouldBe` 3

    describe "`flatten'` returns the list of the `Node` in inorder sequence" $ do

        it "returns [] for the `Empty` tree" $ do
            flatten' Empty `shouldBe` ([] :: [Integer])

        it "returns [3] for the leaf 3" $ do
            flatten' (leaf 3) `shouldBe` [3]

        it "returns [3 4 5] for the tree ((3) 4 (5))" $ do
            flatten' (Node (leaf 3) 4 (leaf 5)) `shouldBe` [3, 4, 5]

    describe "`treeMax` returns the max of the tree" $ do
        
        it "returns 5 for the tree ((3) 4 (5))" $ do
            treeMax ((Node (leaf 3) 4 (leaf 5)) :: Tree Int) `shouldBe` 5

    describe "`eval` returns the value of the `ExprT`" $ do
        
        it "returns 3 for `Lit 3`" $ do
            eval (Lit 3) `shouldBe` 3

        it "returns 7 for `Add 3 4`" $ do
            eval (Add (Lit 3) (Lit 4)) `shouldBe` 7

        it "returns 12 for `Mul 3 4`" $ do
            eval (Mul (Lit 3) (Lit 4)) `shouldBe` 12

    describe "`eval2` returns the value of the `ExprT`" $ do
        
        it "returns 3 for `Lit 3`" $ do
            eval2 (Lit 3) `shouldBe` 3

        it "returns 7 for `Add 3 4`" $ do
            eval2 (Add (Lit 3) (Lit 4)) `shouldBe` 7

        it "returns 12 for `Mul 3 4`" $ do
            eval2 (Mul (Lit 3) (Lit 4)) `shouldBe` 12

    describe "`numLiterals` returns the value of the `ExprT`" $ do
        
        it "returns 1 for `Lit 3`" $ do
            numLiterals (Lit 3) `shouldBe` 1

        it "returns 2 for `Add 3 4`" $ do
            numLiterals (Add (Lit 3) (Lit 4)) `shouldBe` 2

        it "returns 2 for `Mul 3 4`" $ do
            numLiterals (Mul (Lit 3) (Lit 4)) `shouldBe` 2

    describe "`prod`" $ do
        
        it "returns 1 * 5 * 8 * 23 * 423 * 99" $ do
            prod `shouldBe` 1 * 5 * 8 * 23 * 423 * 99

    describe "(Monoid a, Monoid b) => Monoid (a, b)" $ do
        
        it "[(1, 2), (3, 4), (5, 6), (7, 8)] => (16, 20)" $ do
            mconcat [(Sum 1, Sum 2), (Sum 3, Sum 4), (Sum 5, Sum 6), (Sum 7, Sum 8)] `shouldBe` (Sum 16, Sum 20)

    describe "Bool a => Monoid a" $ do
        
        it "bool" $ do
            mconcat [False, True, False] `shouldBe` True

    describe "JoinList" $ do
        let empty = JEmpty :: JoinList Size String
        let single x = JSingle (1 :: Size) x
        let jl = (single 'y' +++ (single 'e' +++ single 'a')) +++ single 'h'
        
        it "(+++)" $ do
            let j = JAppend (Product 210)
                   (JAppend (Product 30) (JSingle (Product 5) 'y')
                                       (JAppend (Product 6) (JSingle (Product 2) 'e')
                                                          (JSingle (Product 3) 'a')))
                    (JSingle (Product 7) 'h')
            let j1 = JAppend (Product 30) (JSingle (Product 5) 'y')
                                                   (JAppend (Product 6) (JSingle (Product 2) 'e')
                                                                      (JSingle (Product 3) 'a'))
            let j2 = (JSingle (Product 7) 'h')

            j1 +++ j2 `shouldBe` j

        describe "indexJ" $ do
            context "with an Empty JoinList" $ do
                it "returns Nothing" $ do
                    indexJ 1 empty `shouldBe` Nothing

            context "with a Single JoinList" $ do
                context "with a 0 index" $ do
                    it "returns the element" $ do
                        let s = single 'a'
                        indexJ 0 s `shouldBe` Just 'a'

                context "with an index /= 0" $ do
                    it "returns Nothing" $ do
                        let s = single 'a'
                        indexJ 1 s `shouldBe` Nothing

            context "with an Append JoinList" $ do
                it "returns the nth element" $ do
                    indexJ (-1) jl `shouldBe` Nothing
                    indexJ 0 jl `shouldBe` (jlToList jl) !!? 0
                    indexJ 1 jl `shouldBe` (jlToList jl) !!? 1
                    indexJ 2 jl `shouldBe` (jlToList jl) !!? 2
                    indexJ 3 jl `shouldBe` (jlToList jl) !!? 3
                    indexJ 4 jl `shouldBe` (jlToList jl) !!? 4
                        
        describe "dropJ" $ do
            context "with an Empty JoinList" $ do
                it "returns Nothing" $ do
                    dropJ 0 empty `shouldBe` JEmpty
                    
            context "with a Single JoinList" $ do
                context "with a 0 index" $ do
                    it "returns the original JoinList" $ do
                        let s = single 'a'
                        dropJ 0 s `shouldBe` s

                context "with an index > 0" $ do
                    it "returns Nothing" $ do
                        let s = single 'a'
                        dropJ 1 s `shouldBe` JEmpty
        
            context "with an Append JoinList" $ do
                    it "drops first n elements" $ do
                        jlToList (dropJ (-1) jl) `shouldBe` jlToList jl
                        jlToList (dropJ 0 jl) `shouldBe` drop 0 (jlToList jl)
                        jlToList (dropJ 1 jl) `shouldBe` drop 1 (jlToList jl)
                        jlToList (dropJ 2 jl) `shouldBe` drop 2 (jlToList jl)
                        jlToList (dropJ 3 jl) `shouldBe` drop 3 (jlToList jl)
                        jlToList (dropJ 4 jl) `shouldBe` jlToList JEmpty

        describe "takeJ" $ do
            context "with an Empty JoinList" $ do
                it "returns Nothing" $ do
                    takeJ 1 empty `shouldBe` JEmpty
                    
            context "with a Single JoinList" $ do
                context "with a 0 index" $ do
                    it "returns the Empty" $ do
                        let s = single 'a'
                        takeJ 0 s `shouldBe` JEmpty

                context "with an index > 0" $ do
                    it "returns the Single JoinList" $ do
                        let s = single 'a'
                        takeJ 1 s `shouldBe` s
        
            context "with an Append JoinList" $ do
                    it "takes first n elements" $ do
                        jlToList (takeJ (-1) jl) `shouldBe` jlToList JEmpty
                        jlToList (takeJ 0 jl) `shouldBe` take 0 (jlToList jl)
                        jlToList (takeJ 1 jl) `shouldBe` take 1 (jlToList jl)
                        jlToList (takeJ 2 jl) `shouldBe` take 2 (jlToList jl)
                        jlToList (takeJ 3 jl) `shouldBe` take 3 (jlToList jl)
                        jlToList (takeJ 4 jl) `shouldBe` take 4 (jlToList jl)
    
    describe "Exercise 3 - Scrabble" $ do
        describe "score" $ do
            it "returns Score of the char" $ do
                score 'a' `shouldBe` Score 1
                score 'z' `shouldBe` Score 10
                score '0' `shouldBe` Score 0
                score ' ' `shouldBe` Score 0
                score '!' `shouldBe` Score 0
        
        describe "scoreString" $ do
            it "returns Score of the string" $ do
                scoreString "yay " `shouldBe` Score 9
                scoreString "haskell!" `shouldBe` Score 14
        
        describe "scoreLine" $ do
            it "returns JoinList of score string" $ do
                scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` JAppend (Score 23) (JSingle (Score 9) "yay ") (JSingle (Score 14) "haskell!")
