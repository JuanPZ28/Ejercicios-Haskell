import System.IO

-- Calcula el precio de compra de las llantas según la cantidad
compra :: Int -> Int
compra n
  | n <= 0    = 0  -- Aquí evitamos los negativos y el caso de 0
  | n < 5     = n * 150000
  | otherwise = n * 138000

-- Imprime la información de la compra de llantas
icoLlantas :: String -> Int -> IO ()
icoLlantas nom nLlantas =
  if nLlantas <= 0
    then putStrLn "Número de llantas inválido"
    else putStrLn ("Nombre: " ++ nom ++ 
                   ", Llantas: " ++ show nLlantas ++ 
                   ", Precio: " ++ show (compra nLlantas))

-- Punto de entrada con manejo de errores en la entrada de usuario
main :: IO ()
main = do
  putStrLn "Ingrese su nombre:"
  nom <- getLine
  putStrLn "Ingrese número de llantas:"
  entrada <- getLine
  
  -- Manejo de posibles errores en la conversión a Int
  let nLlantas = case reads entrada :: [(Int, String)] of
                    [(num, "")] -> num
                    _ -> -1 -- Valor inválido
  
  icoLlantas nom nLlantas