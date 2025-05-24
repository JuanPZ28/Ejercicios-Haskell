import Control.Monad (replicateM)
import Text.Printf (printf)

precioPorKilo :: Float
precioPorKilo = 2.5  -- precio del kilo de naranjas

calcularPago :: Float -> Float
calcularPago kilos
  | kilos > 10 = kilos * precioPorKilo * 0.85
  | otherwise  = kilos * precioPorKilo

leerCompra :: Int -> IO Float
leerCompra i = do
  putStrLn $ "Cliente " ++ show i ++ ", Kilos comprados: "
  input <- getLine
  let kilos = read input :: Float
  return kilos

main :: IO ()
main = do
  kilosList <- mapM leerCompra [1..15]
  let pagos = map calcularPago kilosList
  putStrLn "\n Pagos por cliente: "
  mapM_ (\(i :: Int, pago) -> printf "Cliente #%d: $%.2f\n" i pago) (zip [1..15] pagos)
  let total = sum pagos
  printf "\nTotal recaudado por la tienda: $%.2f\n" total
