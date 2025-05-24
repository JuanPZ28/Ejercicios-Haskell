import Control.Monad (replicateM)
import Text.Printf (printf)

-- Tipos de datos para facilitar clasificación
data Categoria = Niño | Adolescente | Joven | Adulto | Viejo deriving (Show, Eq, Ord)

-- Clasifica según edad
clasificarEdad :: Int -> Categoria
clasificarEdad edad
    | edad <= 12  = Niño
    | edad <= 17  = Adolescente
    | edad <= 29  = Joven
    | edad <= 49  = Adulto
    | otherwise   = Viejo

leerPersona :: IO (Categoria, Float)
leerPersona = do
    putStrLn "Ingrese la edad:"
    edadStr <- getLine
    putStrLn "Ingrese el peso:"
    pesoStr <- getLine
    let edad = read edadStr :: Int
        peso = read pesoStr :: Float
        categoria = clasificarEdad edad
    return (categoria, peso)

agruparPorCategoria :: [(Categoria, Float)] -> [(Categoria, [Float])]
agruparPorCategoria datos = foldr insertarCategoria [] datos
  where
    insertarCategoria (cat, peso) [] = [(cat, [peso])]
    insertarCategoria (cat, peso) ((c, ps):rest)
        | cat == c  = (c, peso:ps) : rest
        | otherwise = (c, ps) : insertarCategoria (cat, peso) rest

promedio :: [Float] -> Float
promedio xs = sum xs / fromIntegral (length xs)

imprimirPromedios :: [(Categoria, [Float])] -> IO ()
imprimirPromedios categorias = do
    putStrLn "Promedio de peso por categoría"
    mapM_ imprimir categorias
  where
    imprimir (cat, pesos) = 
        printf "%-12s: %.2f kg\n" (show cat) (promedio pesos)

main :: IO ()
main = do
    putStrLn "Ingrese la edad y peso de 50 personas:"
    personas <- replicateM 50 leerPersona
    let agrupados = agruparPorCategoria personas
    imprimirPromedios agrupados
