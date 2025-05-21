-- Función que pide n temperaturas y las guarda en una lista
pedirNumeros :: Int -> IO [Int]
pedirNumeros 0 = return []
pedirNumeros n = do
    putStrLn $ "Ingresa la temperatura (" ++ show n ++ " restantes):"
    input <- getLine
    let numero = read input :: Int
    resto <- pedirNumeros (n - 1)
    return (numero : resto)

main :: IO ()
main = do
    putStrLn "Vas a ingresar 28 temperaturas (una por cada día):"
    temperaturas <- pedirNumeros 28
    putStrLn "Las temperaturas ingresadas fueron:"
    print temperaturas

    let suma = sum temperaturas
        promedio :: Double
        promedio = fromIntegral suma / fromIntegral (length temperaturas)

        maxima = maximum temperaturas
        minima = minimum temperaturas

        -- Buscar la posición del primer día con temperatura máxima
        dias = [1..28]
        paresDiaTemp = zip dias temperaturas

        diaMax = fst (head (filter (\(_, t) -> t == maxima) paresDiaTemp))
        diaMin = fst (head (filter (\(_, t) -> t == minima) paresDiaTemp))

    putStrLn $ "Temperatura promedio del mes: " ++ show promedio ++ "°C"
    putStrLn $ "Temperatura máxima: " ++ show maxima ++ "°C (Día " ++ show diaMax ++ ")"
    putStrLn $ "Temperatura mínima: " ++ show minima ++ "°C (Día " ++ show diaMin ++ ")"
