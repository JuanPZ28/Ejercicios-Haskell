import System.IO
esClave :: Int -> Int
esClave c = if c == 1 || c == 2 then c else 1
descuento :: Int -> Float -> Float
descuento c p = if c == 1 then p * 0.9 else p * 0.8

-- Función para mostrar el artículo con su precio con descuento
articulo :: String -> Int -> Float -> IO ()
articulo n c p = 
  if p < 0
    then putStrLn "Precio inválido"
    else putStrLn ("Nombre: " ++ n ++ 
                   ", Clave: " ++ show (esClave c) ++ 
                   ", Precio: " ++ show (descuento (esClave c) p))

-- Punto de entrada con entrada de usuario
main :: IO ()
main = do
  putStrLn "Ingrese el nombre del artículo:"
  nombre <- getLine
  putStrLn "Ingrese la clave (1 o 2):"
  claveStr <- getLine
  putStrLn "Ingrese el precio:"
  precioStr <- getLine
  
  let clave = read claveStr :: Int
      precio = read precioStr :: Float
  
  articulo nombre clave precio