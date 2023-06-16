module Lists99 where

{-
 Exercises defined in wiki.haskell.org/99_questions/1_to_10
-}

-- Problem 1
myLast :: [x] -> x
myLast (h:[]) = h
myLast (_:t) = myLast t

-- Problem 2
myButLast :: [x] -> x
myButLast (h:[x]) = h
myButLast (h:t) = myButLast t

-- Problem 3
elementAt :: [x] -> Int -> x
elementAt (h:t) 1 = h
elementAt (h:t) i = elementAt t (i - 1)

-- Problem 4
myLength :: [x] -> Int
myLength [] = 0
myLength (h:t) = 1 + myLength t

-- Problem 5
myReverse :: [x] -> [x]
myReverse [] = []
myReverse (h:t) = myReverse t ++ [h]

-- Problem 6
isPalindrome :: Eq x => [x] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (h:t)) = flatten h ++ flatten (List t)

-- Problem 8
compress :: Eq x => [x] -> [x]
compress [] = []
compress (h:[]) = [h]
compress (h:t)
    | h == head t = compress t
    | otherwise = h : compress t

-- Problem 9
-- TODO

-- Problem 10
-- TODO

main :: IO()
main = do
    print(myLast [1, 2, 3, 4])
    print(myButLast [1, 2, 3, 4])
    print(elementAt ['a', 'b', 'c', 'd', 'e'] 4)
    print(myLength [1, 2, 3, 4, 5, 6])
    print(myReverse [1, 2, 3, 4])
    print(isPalindrome [1, 2, 3, 2, 1])
    print(flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    print(compress "aaaabccaadeeee")

