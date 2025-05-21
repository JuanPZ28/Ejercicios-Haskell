existeNumeroIgualASumaDelResto :: [Int] -> Bool
existeNumeroIgualASumaDelResto lista =
    let sumaTotal = sum lista
    in any (\x -> 2 * x == sumaTotal) lista

main :: IO ()
main = do
    let vector = [3, 1, 2, 6]
    if existeNumeroIgualASumaDelResto vector
        then putStrLn "Sí existe un número igual a la suma del resto."
        else putStrLn "No existe tal número."
