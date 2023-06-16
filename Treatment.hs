module Treatment where

sr :: String -> String
sr s = "Sr. " ++ s 

sra :: String -> String
sra s = "Sra. " ++ s

srta :: String -> String
srta s = "Srta. " ++ s

treat :: [String] -> (String -> String) -> [String]
treat [] _ = []
treat (h:t) f = f h : treat t f

treat_funs :: [String] -> [(String -> String)] -> [[String]]
treat_funs _ [] = []
treat_funs l (h:t) = treat l h : treat_funs l t

names :: [String]
names = ["Jandre", "Simon", "Devede"]

funs :: [(String -> String)]
funs = [sr, sra, srta]

main :: IO()
main = do
    print(treat_funs names funs)

