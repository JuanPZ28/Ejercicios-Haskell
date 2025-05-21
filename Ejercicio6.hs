main = do
    let miLista :: [Int]
        miLista = [42, 17, 83, 9, 56, 21, 74, 3, 68, 90,
                   12, 37, 5, 61, 28, 49, 77, 15, 36, 81,
                   6, 24, 59, 70, 19, 95, 32, 8, 45, 63]

        pares = filter even miLista
        suma = foldl (+) 0 miLista
        promedio :: Double
        promedio = fromIntegral suma / fromIntegral (length miLista)  -- División correcta

    putStrLn $ "Números pares: " ++ show pares
    putStrLn $ "Suma total: " ++ show suma
    putStrLn $ "Promedio: " ++ show promedio
