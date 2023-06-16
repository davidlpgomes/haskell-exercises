module Lists99 where

{-
 Exercises defined in wiki.haskell.org/99_questions/1_to_10
-}

-- Problem 1
myLast :: [x] -> x
myLast (h:[]) = h
myLast (h:t) = myLast t

-- Problem 2
myButLast :: [x] -> x
myButLast (h:[x]) = h
myButLast (h:t) = myButLast t

-- Problem 3
elementAt :: [x] -> Int -> x
elementAt (h:t) 1 = h
elementAt (h:t) i = elementAt t (i - 1)

main :: IO()
main = do
    print(myLast [1, 2, 3, 4])
    print(myButLast [1, 2, 3, 4])
    print(elementAt ['a', 'b', 'c', 'd', 'e'] 4)

