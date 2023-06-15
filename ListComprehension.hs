module ListComprehension where

-- Define [0, 3, 6, 9, 12, 15]
list_a :: [Int]
list_a = [3 * x | x <- [0..5]]

-- Define [[1], [2], [3], [4], [5]]
list_b :: [[Int]]
list_b = [[x] | x <- [1..5]]

main :: IO()
main = do
    print(list_a)
    print(list_b)

