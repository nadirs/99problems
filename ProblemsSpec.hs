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

    describe "elementAt" $ do
        it "finds the K'th element of a list" $ do
            myElementAt [3,-2,4,9,1] 3 `shouldBe` 4
            myElementAt "ciao" 2 `shouldBe` 'i'
            myElementAt [[4],[22],[9]] 1 `shouldBe` [4]

    describe "myLength" $ do
        it "finds the number of elements of a list" $ do
            myLength [123, 456, 789] `shouldBe` 3
            myLength "Hello, world!" `shouldBe` 13

    describe "myReverse" $ do
        it "reverses a list" $ do
            myReverse "A man, a plan, a canal, panama!" `shouldBe`
                "!amanap ,lanac a ,nalp a ,nam A"
            myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

    describe "myIsPalindrome" $ do
        it "finds out whether a list is a palindrome" $ do
            myIsPalindrome [1,2,3] `shouldBe` False
            myIsPalindrome "madamimadam" `shouldBe` True
            myIsPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

    describe "myFlatten" $ do
        it "flatten a nested list structure" $ do
            myFlatten (Elem 5) `shouldBe` [5]
            myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe`
                [1,2,3,4,5]

    describe "myCompress" $ do
        it "eliminates consecutive duplicates of list elements" $ do
            myCompress "aaaabccaadeee" `shouldBe` "abcade"

    describe "myPack" $ do
        it "packs consecutive duplicates of list elements into sublists" $ do
            myPack "" `shouldBe` []
            myPack "a" `shouldBe` ["a"]
            myPack "ab" `shouldBe` ["a", "b"]
            myPack "aa" `shouldBe` ["aa"]
            myPack "aab" `shouldBe` ["aa", "b"]
            myPack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

    describe "myRLEncode" $ do
        it "does run-length encoding of a list" $ do
            myRLEncode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
            
    describe "myRLEncodeMod" $ do
        it "RLEs unless an element has no duplicates (it is simply copied into the result list)" $ do
            myRLEncodeMod "aaaabccaadeeee" `shouldBe`
                [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    describe "myDecodeRLEMod" $ do
        it "decodes a run-length encoded list" $ do
            myDecodeRLEMod [Multiple 4 'a',Single 'b',Multiple 2 'c',
                           Multiple 2 'a',Single 'd',Multiple 4 'e']
                           `shouldBe` "aaaabccaadeeee"

    describe "myRLEncodeDirect" $ do
        it "implements the so-called run-length encoding data compression method directly" $ do
            myRLEncodeDirect "aaaabccaadeeee" `shouldBe`
                [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    describe "myDuplicate" $ do
        it "duplicates the elements of a list" $ do
            myDuplicate [1, 2, 3] `shouldBe` [1,1,2,2,3,3]

    describe "myReplicate" $ do
        it "replicate the elements of a list a given number of times" $ do
            myReplicate "abc" 3 `shouldBe` "aaabbbccc"

    describe "myDropEvery" $ do
        it "drops every N'th element from a list" $ do
            myDropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    describe "mySplit" $ do
        it "splits a list into two parts, given the length of the first part" $ do
            mySplit "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    describe "mySlice" $ do
        it "extracts a slice from a list, given two inclusive limits, starting counting at 1" $ do
            mySlice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

    describe "myRotate" $ do
        it "rotates a list N places to the left" $ do
            myRotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
            myRotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

    describe "myRemoveAt" $ do
        it "removes the K'th element from a list" $ do
            myRemoveAt 2 "abcd"`shouldBe` ('b',"acd")
