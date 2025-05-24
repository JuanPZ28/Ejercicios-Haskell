import System.IO

-- Función para calcular el máximo, mínimo y moda de una lista
mayorMenorModa :: [Int] -> String
mayorMenorModa lista = "Máximo: " ++ show (maximum lista) ++ "\n" ++
                       "Mínimo: " ++ show (minimum lista) ++ "\n" ++
                       "Moda: " ++ show (calcularModa lista)

-- Función auxiliar para calcular la moda
calcularModa :: [Int] -> Int
calcularModa lista = snd $ maximum [(length (filter (==x) lista), x) | x <- lista]

-- Función para leer una lista de números con validación de entrada
leerNumeros :: Int -> IO [Int]
leerNumeros n = leerNumerosAux n 1

leerNumerosAux :: Int -> Int -> IO [Int]
leerNumerosAux 0 _ = return []
leerNumerosAux n contador = do
  putStrLn $ "Ingresa el número #" ++ show contador ++ ":"
  entrada <- getLine
  case reads entrada :: [(Int, String)] of
    [(numero, "")] -> do
      resto <- leerNumerosAux (n - 1) (contador + 1)
      return (numero : resto)
    _ -> do
      putStrLn "Entrada inválida, por favor ingrese un número entero."
      leerNumerosAux n contador

-- Punto de entrada
main :: IO ()
main = do
  putStrLn "Ingrese la longitud del vector:"
  entrada <- getLine
  case reads entrada :: [(Int, String)] of
    [(longitud, "")] -> do
      lista <- leerNumeros longitud
      putStrLn "\nLista generada:"
      putStrLn $ show lista
      putStrLn "\nResultados:"
      putStrLn $ mayorMenorModa lista
    _ -> putStrLn "Entrada inválida, por favor ingrese un número entero."