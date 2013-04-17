-- Specification for the 99 Haskell problems

import Test.Hspec
import Problems

main = hspec $ do
    describe "myLast" $ do
        it "finds last element of a list" $ do
            myLast [-5,4,2,6] `shouldBe` 6
            myLast "ciao" `shouldBe` 'o'
            myLast [[4],[22],[9]] `shouldBe` [9]

    describe "myButLast" $ do
        it "finds last but one element of a list" $ do
            myButLast [-5,4,2,6] `shouldBe` 2
            myButLast "ciao" `shouldBe` 'a'
            myButLast [[4],[22],[9]] `shouldBe` [22]

    describe "elemAt" $ do
        it "finds the K'th element of a list" $ do
            myElementAt [3,-2,4,9,1] 3 `shouldBe` 4
            myElementAt "ciao" 2 `shouldBe` 'i'
            myElementAt [[4],[22],[9]] 1 `shouldBe` [4]
