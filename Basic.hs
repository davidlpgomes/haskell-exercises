module Basic where

calcular_reajuste :: (Float) -> (Float)
calcular_reajuste s = s * 1.25

calcular_media_ponderada :: (Float) -> (Float) -> (Float) -> (Float) -> (Float) -> (Float) -> (Float)
calcular_media_ponderada a b c pa pb pc = (a * pa + b * pb + c * pc) / (pa + pb + pc)

converte_temp :: (Float) -> (Float)
converte_temp c = c * (9.0 / 5.0) + 32.0

calcular_idade :: (Int) -> (Int)
calcular_idade nasc = 2032 - nasc

calcular_minutos :: (Int) -> (Int) -> (Int)
calcular_minutos h m  = h * 60 + m

soma_dos_quadrados :: (Int) -> (Int) -> (Int) -> (Int)
soma_dos_quadrados a b c = a * a + b * b + c * c

calcular_salario :: (Float) -> (Float)
calcular_salario s = s * 0.98

main :: IO()
main = do
    print(calcular_reajuste 10)
    print(calcular_media_ponderada 0 5 10 0 0 1)
    print(converte_temp 32)
    print(calcular_idade 2000)
    print(calcular_minutos 2 30)
    print(soma_dos_quadrados 1 2 3)
    print(calcular_salario 1200)

