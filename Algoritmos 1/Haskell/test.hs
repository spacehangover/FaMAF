cuentaVacia :: [[a]] -> Int
cuentaVacia [] = 0
cuentaVacia ([]:xss) = 1 + cuentaVacia xss
cuentaVacia (_:xss) = cuentaVacia xss


noPasa :: [Int] -> Int -> [Int]
noPasa [] _ = []
noPasa (x:xs) n
  | x == n    = noPasa xs n
  | otherwise = x : noPasa xs n


totalChars :: [String]->Int
totalChars [] = 0
totalChars(xs:xss) = length(xs)  +totalChars xss

tramoInicialUno :: [[Int]] -> [[Int]]
tramoInicialUno [] = []
tramoInicialUno(x:xs)| x==[]= x : tramoInicialUno xs
                     | otherwise = [head(x)] : tramoInicialUno xs