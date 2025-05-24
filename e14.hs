tabla :: Int -> [String]
tabla n = [show n ++ " x " ++ show i ++ " = " ++ show (n * i) | i <- [1..10]]

main :: IO ()
main = do
  mapM_ imprimirTabla [1..10]

imprimirTabla :: Int -> IO ()
imprimirTabla n = do
  putStrLn $ "\nTabla del " ++ show n ++ ":"
  mapM_ putStrLn (tabla n)
