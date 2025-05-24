esPrimo :: Int -> Bool
esPrimo n
  | n < 2     = False
  | otherwise = null [x | x <- [2..n-1], n `mod` x == 0]

main :: IO ()
main = do
  putStrLn "Ingrese un número:"
  input <- getLine
  let num = read input :: Int
  let primos = [x | x <- [2..num], esPrimo x]
  putStrLn $ "Números primos hasta " ++ show num ++ ":"
  print primos
