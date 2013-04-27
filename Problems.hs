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

-- 11 Modified run-length encoding
data RLEList a = Single a | Multiple Int a deriving (Show, Eq)

myRLEncodeMod :: (Eq a) => [a] -> [RLEList a]
myRLEncodeMod = (map encoder) . myRLEncode where
    encoder (1, a) = Single a
    encoder (n, a) = Multiple n a

-- 12 Decode a run-length encoded list
myDecodeRLEMod :: (Eq a) => [RLEList a] -> [a]
myDecodeRLEMod [] = []
myDecodeRLEMod (x:xs) = (decoder x) ++ myDecodeRLEMod xs where
    decoder (Single a) = [a]
    decoder (Multiple n a)
        | n > 1     = [a] ++ decoder (Multiple (n-1) a)
        | otherwise = decoder (Single a)

-- 13 Run-length encoding of a list (direct solution)
myRLEncodeDirect :: (Eq a) => [a] -> [RLEList a]
myRLEncodeDirect [] = []
myRLEncodeDirect (x:xs) = helpRLE 1 x xs where
    helpRLE n a [] = [makeRLE n a]
    helpRLE n a (x:xs)
        | a == x    = helpRLE (n+1) a xs
        | otherwise = (makeRLE n a) : (helpRLE 1 x xs)
    makeRLE 1 a = Single a
    makeRLE n a = Multiple n a

-- 14 Duplicate the elements of a list
myDuplicate :: [a] -> [a]
myDuplicate = (`myReplicate` 2)

-- 15 Replicate the elements of a list a given number of times
myReplicate :: [a] -> Int -> [a]
myReplicate xs n = foldr (repli n) [] xs where
    repli n = (\ z -> replin n z)
    replin 0 _ z = z
    replin n x z = replin (n-1) x (x:z)

-- 16 Drop every N'th element from a list
myDropEvery :: [a] -> Int -> [a]
myDropEvery xs interval = helpDrop xs interval where
    helpDrop [] _ = []
    helpDrop (x:xs) 1 = helpDrop xs interval
    helpDrop (x:xs) n = x : helpDrop xs (n - 1)

-- 17 Split a list into two parts, given the length of the first part
mySplit :: [a] -> Int -> ([a],[a])
mySplit xs n = (take n xs, drop n xs)

-- 18 Extract a slice from a list
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs lo up = slicer xs where
    slicer = drop (lo-1) . take (up)

-- 19 Rotate a list N places to the left
myRotate :: [a] -> Int -> [a]
myRotate [] _ = []
myRotate xs n = listify $ mySplit xs $ pos `mod` length xs where
    pos = if n < 0 then (length xs) + n else n
    listify (xs,ys) = ys ++ xs

-- 20 Remove the K'th element from a list
myRemoveAt :: Int -> [a] -> (a,[a])
myRemoveAt k xs = ((xs !! (k-1)), (take (k-1) xs) ++ (drop k xs))
