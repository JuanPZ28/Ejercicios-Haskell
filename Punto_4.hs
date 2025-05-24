import Data.List (sort)

-- Función para ordenar de mayor a menor
ordenarMayorAMenor :: [Int] -> [Int]
ordenarMayorAMenor = reverse . sort

-- Función para ordenar de menor a mayor
ordenarMenorAMayor :: [Int] -> [Int]
ordenarMenorAMayor = sort

-- Función para leer 15 números del usuario con validación
leerNumeros :: Int -> IO [Int]
leerNumeros 0 = return []
leerNumeros n = do
  putStrLn $ "Ingresa el número #" ++ show (16 - n) ++ ":"
  entrada <- getLine
  case reads entrada :: [(Int, String)] of
    [(numero, "")] -> do
      resto <- leerNumeros (n - 1)
      return (numero : resto)
    _ -> do
      putStrLn "Entrada inválida, por favor ingrese un número entero."
      leerNumeros n

-- Punto de entrada
main :: IO ()
main = do
  putStrLn "Ingrese 15 números:"
  lista <- leerNumeros 15
  putStrLn "\nLista generada:"
  print lista
  putStrLn "\nOrden de mayor a menor:"
  print (ordenarMayorAMenor lista)
  putStrLn "\nOrden de menor a mayor:"
  print (ordenarMenorAMayor lista)