-- Ninety-Nine Haskell Problems
-- link: http://www.haskell.org/haskellwiki/99_questions/

module Problems where

--  1 Find the last element of a list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

--  2 Find the last but one element of a list
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

--  3 Find the K'th element of a list. The first element in the list is number 1.
myElementAt :: [a] -> Int -> a
myElementAt (x:_) 1 = x
myElementAt (_:xs) i = myElementAt xs (i-1)

--  4 Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = (myLength xs) + 1

--  5 Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--  6 Find out whether a list is a palindrome
myIsPalindrome :: (Eq a) => [a] -> Bool
myIsPalindrome [] = True
myIsPalindrome [_] = True
myIsPalindrome (x:xs) = (myIsPalindrome (init xs)) && (x == (last xs))

--  7 Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs))

--  8 Eliminate consecutive duplicates of list elements
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:ys@(y:zs))
    | x == y    = myCompress ys
    | otherwise = x : myCompress ys

--  9 Pack consecutive duplicates of list elements into sublists
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
myPack xs = packer [] xs where
    packer zs [] = zs : myPack []
    packer [] (x:xs) = packer [x] xs
    packer zs@(z:zzs) xs@(x:xxs)
        | z == x    = packer (x:zs) xxs
        | otherwise = zs : myPack xs

-- 10 Run-length encoding of a list
myRLEncode :: (Eq a) => [a] -> [(Int,a)]
myRLEncode [] = []
myRLEncode xs = zip (map length pack) (map head pack) where
    pack = (myPack xs)
