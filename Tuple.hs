module Tuple where

-- Int -> return (double, triple, quadruple, quintuple)
mult :: Int -> (Int, Int, Int, Int)
mult x = (2 * x, 3 * x, 4 * x, 5 * x)

-- Int -> (half, odd or even)
half_oe :: Int -> (Int, String)
half_oe x = (div x 2, y)
    where
    y
     | mod x 2 == 0 = "Even"
     | otherwise = "Odd"

-- [Int] -> (positives sum, negatives product)
sp :: [Int] -> (Int, Int)
sp [] = (0, 1)
sp (h:t)
    | h > 0 = (s + h, p)
    | otherwise = (s, p * h)
    where
        (s, p) = sp t

turist :: (Int) -> [(String, Int, String)] -> [(String, Int)]
turist r l = [(x, y) | (x, y, z) <- l, y > r && (z == "museu" || z == "parque")]

main :: IO()
main = do
    print(mult 2)
    print(half_oe 10)
    print(sp [-2, -1, 2, 3])
    print(turist 6 [
        ("Rua 24 Horas", 5, "rua"),
        ("Tangua", 10, "parque"),
        ("MON", 7, "museu"),
        ("Mercado Municipal", 8, "mercado"),
        ("Jardim Botanico", 10, "parque"),
        ("Museu Paranaense", 6, "museu"),
        ("Feira do Largo da ordem", 9, "feira"),
        ("Barigui", 10, "parque"),
        ("Rua das Flores", 7, "rua")
        ])

