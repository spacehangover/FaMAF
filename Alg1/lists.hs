soloPares :: [Int] -> [Int]
soloPares xs = filter even xs

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 xs = [x | x <- xs, x > 10]

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue n xs = [x | x <- xs, x > n]

sumar1 :: Int -> Int
sumar1 x = x + 1
numSumados xs = map sumar1 xs

duplica :: Int -> Int
duplica x = x * 2
numDuplicados xs = map duplica xs

multiplica :: Int -> Int -> Int
duplica n x = x * n
numDupliN n xs = map  xs

