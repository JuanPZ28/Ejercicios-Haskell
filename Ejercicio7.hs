-- Función que pide n números reales y los guarda en una lista
pedirNumeros :: Int -> IO [Double]
pedirNumeros 0 = return []
pedirNumeros n = do
    putStrLn $ "Ingresa un número real (" ++ show n ++ " restantes):"
    input <- getLine
    let numero = read input :: Double
    resto <- pedirNumeros (n - 1)
    return (numero : resto)

main :: IO ()
main = do
    putStrLn "Vas a ingresar 10 números reales:"
    numeros <- pedirNumeros 10
    putStrLn "Los números ingresados son:"
    print numeros

    let suma = sum numeros
        promedio = suma / fromIntegral (length numeros)
        mayores = length (filter (> promedio) numeros)
        menores = length (filter (<= promedio) numeros)

    putStrLn $ "Promedio: " ++ show promedio
    putStrLn $ "Cantidad de números mayores al promedio: " ++ show mayores
    putStrLn $ "Cantidad de números menores o iguales al promedio: " ++ show menores
