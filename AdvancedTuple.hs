module AdvancedTuple where

frst :: (String, String, Char) -> String
frst (x, _, _) = x

scnd :: (String, String, Char) -> String
scnd (_, y, _) = y

thrd :: (String, String, Char) -> Char
thrd (_, _, z) = z

base :: Int -> (String, String, Char)
base x
    | x == 1 = ("joao", "mestre", 'm')
    | x == 2 = ("jonas", "doutor", 'm')
    | x == 3 = ("joice", "mestre", 'f')
    | x == 4 = ("janete", "doutor", 'f')
    | x == 5 = ("jocileide", "doutor", 'f')
    | otherwise = ("ninguem", "nada", 'x')

contMestre :: Int -> Int 
contMestre 0 = 0
contMestre x
    | scnd (base x) == "mestre" = 1 + contMestre (x - 1)
    | otherwise = contMestre (x - 1)

contDoc :: Int -> Int 
contDoc 0 = 0
contDoc x
    | scnd (base x) == "doutor" = 1 + contDoc (x - 1)
    | otherwise = contDoc (x - 1)

contMd :: String -> Int -> Int
contMd _ 0 = 0
contMd t x
    | scnd (base x) == t = 1 + contMd t (x - 1)
    | otherwise = contMd t (x - 1)

cont :: String -> Char -> Int -> Int
cont _ _ 0 = 0
cont t s x
    | s == sex && t == titulo = 1 + cont t s (x - 1)
    | otherwise = cont t s (x - 1)
    where
        (_, titulo, sex) = base x

main :: IO()
main = do
    print(frst ("abc", "cba", 'c'))
    print(scnd ("abc", "cba", 'c'))
    print(thrd ("abc", "cba", 'c'))
    print(contMestre 10)
    print(contDoc 10)
    print(contMd "mestre" 10)
    print(contMd "doutor" 10)
    print(contMd "nada" 10)
    print(cont "doutor" 'f' 10)
