module FloatFilter where

ispositive :: Float -> Bool
ispositive x = if x > 0 then True else False

isnegative :: Float -> Bool
isnegative x = if x < 0 then True else False

iszero :: Float -> Bool
iszero x = if x == 0 then True else False

filterf :: [Float] -> (Float -> Bool) -> [Float]
filterf [] _ = []
filterf (h:t) f
    | cond == True = h : filterf t f
    | otherwise = filterf t f
    where
        cond = f h

main :: IO()
main = do
    print(ispositive 10)
    print(ispositive (-10))
    print(isnegative 10)
    print(isnegative (-10))
    print(iszero 0)
    print(iszero 10)
    print(filterf [-5, -2, 0, 0, 2, 5] ispositive)
    print(filterf [-5, -2, 0, 0, 2, 5] isnegative)
    print(filterf [-5, -2, 0, 0, 2, 5] iszero)

