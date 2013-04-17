-- Ninety-Nine Haskell Problems
-- link: http://www.haskell.org/haskellwiki/99_questions/

module Problems where

--  1 Find the last element of a list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--  2 Find the last but one element of a list
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs

--  3 Find the K'th element of a list. The first element in the list is number 1.
myElementAt :: [a] -> Int -> a
myElementAt (x:_) 1 = x
myElementAt (x:xs) i = myElementAt xs (i-1)

