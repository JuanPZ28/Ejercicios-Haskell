main :: IO ()
main = do
    let miLista :: [Int]
        miLista = take 30 [2,4..]

        suma = sum miLista
        promedio :: Double
        promedio = fromIntegral suma / fromIntegral (length miLista)

    putStrLn $ "Lista de los primeros 30 números pares: " ++ show miLista
    putStrLn $ "Suma total: " ++ show suma
    putStrLn $ "Promedio: " ++ show promedio

