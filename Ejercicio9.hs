-- Función que pide 28 temperaturas y los guarda en una lista
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
    putStrLn "Vas a ingresar 28 temperaturas:"
    numeros <- pedirNumeros 28
    putStrLn "Las temperaturas ingresadas fueron:"
    print numeros

    let suma = sum numeros
        promedio :: Double
        promedio = fromIntegral suma / fromIntegral (length numeros)

        maxima = maximum numeros
        minima = minimum numeros

    putStrLn $ "Temperatura promedio del mes es de:" ++ show promedio
    putStrLn $ "La temperatura maxima fue:"
    print maxima
    putStrLn $ "La temperatura minima fue: " 
    print minima
