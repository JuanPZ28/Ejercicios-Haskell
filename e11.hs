import Text.Printf (printf)

main :: IO ()
main = mapM_ putStrLn relojDigital

relojDigital :: [String]
relojDigital = [formatear h m s | h <- [0..23], m <- [0..59], s <- [0..59]]

formatear :: Int -> Int -> Int -> String
-- Asegurar siempre dos digitos
formatear h m s = printf "%02d:%02d:%02d" h m s
