import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch7


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
