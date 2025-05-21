-- Función que pide n números y los guarda en una lista
pedirNumeros :: Int -> IO [Int]
pedirNumeros 0 = return []
pedirNumeros n = do
    putStrLn $ "Ingresa un número (" ++ show n ++ " restantes):"
    input <- getLine
    let numero = read input :: Int
    resto <- pedirNumeros (n - 1)
    return (numero : resto)

main :: IO ()
main = do
    putStrLn "Vas a ingresar 10 números:"
    numeros <- pedirNumeros 10
    putStrLn "Los números ingresados son:"
    print numeros

    let suma = sum numeros
        promedio :: Double
        promedio = fromIntegral suma / fromIntegral (length numeros)

        mayores = length (filter (> promedio) (map fromIntegral numeros))
        menores = length (filter (<= promedio) (map fromIntegral numeros))

    putStrLn $ "Promedio: " ++ show promedio
    putStrLn $ "Cantidad de números mayores al promedio: " ++ show mayores
    putStrLn $ "Cantidad de números menores o iguales al promedio: " ++ show menores
