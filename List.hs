module List where

sum_list :: [Int] -> (Int)
sum_list [] = 0
sum_list (h:t) = h + sum_list t

remove_even :: [Int] -> [Int]
remove_even [] = []
remove_even (h:t)
    | mod h 2 == 0 = remove_even t
    | otherwise = h : remove_even t

reverse_list :: [Float] -> [Float]
reverse_list [] = []
reverse_list (h:t) = reverse_list t ++ [h]

only_names_begin_A :: [String] -> [String]
only_names_begin_A [] = []
only_names_begin_A (h:t)
    | head h == 'A' = h : only_names_begin_A t
    | otherwise = only_names_begin_A t

main :: IO()
main = do
    print(sum_list [1, 2, 3])
    print(remove_even [1..10])
    print(reverse_list [1..10])
    print(only_names_begin_A ["Aldo", "Aime", "David", "Simon"])
