import System.IO

-- Función para contar el número de veces que aparece x en una lista
contarApariciones :: Eq a => a -> [a] -> Int
contarApariciones x = length . filter (== x)

-- Función para leer una lista de números con tamaño definido por el usuario
leerNumeros :: Int -> IO [Int]
leerNumeros 0 = return []
leerNumeros n = do
  putStrLn $ "Ingresa el número #" ++ show (n) ++ ":"
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
  putStrLn "Ingrese el tamaño de la lista:"
  entradaTam <- getLine
  case reads entradaTam :: [(Int, String)] of
    [(tam, "")] -> do
      lista <- leerNumeros tam
      putStrLn "\nLista generada:"
      print lista
      putStrLn "\nIngrese el número para contar sus apariciones:"
      entradaNum <- getLine
      case reads entradaNum :: [(Int, String)] of
        [(num, "")] -> putStrLn $ "El número " ++ show num ++ " aparece " ++ show (contarApariciones num lista) ++ " veces."
        _ -> putStrLn "Entrada inválida, por favor ingrese un número entero."
    _ -> putStrLn "Entrada inválida, por favor ingrese un número entero."
