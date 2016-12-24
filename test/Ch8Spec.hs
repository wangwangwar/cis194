import Test.Hspec
import Test.QuickCheck
import System.IO
import Ch8
import Employee
import Data.Monoid


main = hspec $ do

    describe "Ch8" $ do 

        describe "glCons" $ do
            let e = Emp "Stan" 9
            let gl = GL [Emp "Joe" 5] 5

            it "adds an Employee to the GuestList (updating the cached Fun score appropriately)" $ do
                glCons e gl `shouldBe` GL [Emp "Joe" 5, Emp "Stan" 9] 14
    
        describe "Monoid instance for GuestList" $ do
            let gl1 = GL [Emp "Stan" 9] 9
            let gl2 = GL [Emp "Joe" 5] 5

            it "can do monoidic things" $ do
                gl1 `mappend` gl2 `shouldBe` GL [Emp "Stan" 9, Emp "Joe" 5] 14

        describe "moreFun" $ do
            let gl1 = GL [Emp "Stan" 9] 9
            let gl2 = GL [Emp "Joe" 5] 5

            it "returns a funer GuestList" $ do
                moreFun gl1 gl2 `shouldBe` gl1

        describe "treeFold" $ do
            it "folds the Tree" $ do
                let addFun e fun = empFun e + fun
                treeFold addFun 0 testCompany `shouldBe` 46
                
                let lenOfName e l = length (empName e) + l
                treeFold lenOfName 0 testCompany `shouldBe` 29
