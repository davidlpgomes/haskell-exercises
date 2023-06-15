module TypeTuple where

type Nome = String
type Titulo = String
type Genero = Char

type Pesquisador = (Nome, Titulo, Genero)

base :: Int -> (String, String, Char)
base x
    | x == 1 = ("joao", "mestre", 'm')
    | x == 2 = ("jonas", "doutor", 'm')
    | x == 3 = ("joice", "mestre", 'f')
    | x == 4 = ("janete", "doutor", 'f')
    | x == 5 = ("jocileide", "doutor", 'f')
    | otherwise = ("ninguem", "", 'x')

createPesquisadores :: Int -> [Pesquisador]
createPesquisadores 0 = []
createPesquisadores x
    | n /= "ninguem" = (n, t, s) : createPesquisadores (x - 1)
    | otherwise = createPesquisadores (x - 1)
    where
        (n, t, s) = base x

getDoctors :: [Pesquisador] -> [Nome]
getDoctors [] = []
getDoctors ps = [n | (n, t, _) <- ps, t == "doutor"]

main :: IO()
main = do
    print(createPesquisadores 5)
    print(getDoctors (createPesquisadores 5))

