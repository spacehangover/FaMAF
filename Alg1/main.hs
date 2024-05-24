-- 19
esMultiploDe:: Int -> Int -> Bool
esMultiploDe x n = mod n x == 0

-- 20

esBisiesto:: Int -> Bool
esBisiesto y = mod y 400 == 0 || mod y 4 == 0 && mod y 100 /= 100

-- 18 a

signo:: Int -> Int
signo x |x > 0  =   1
        |x < 0  =   -1
        |x == 0  =  0

-- 18 a

entre0y9:: Int -> Bool
entre0y9 x | x <= 9 && x >= 0 = True
           | otherwise = False

-- 18 b

rangoPrecio:: Int -> String
rangoPrecio x | x < 2000 && x > 0 = "Muy barato"
              | x < 0 = "Esto no puede ser"
              | x > 2000 && x < 5000 = "Hay que verlo bien"

-- 18 c

absoluto:: Int ->  Int
absoluto x | x < 0 = x * (-1)
           | x > 0 = x
           | x == 0 = 0

-- 18 d

esMultiplo2:: Int -> Bool
esMultiplo2 x | mod x 2 == 0 = True
              | otherwise = False

-- 21

maximo3:: Int -> Int -> Int -> Int
maximo3 x y z = max x (max y z)

minimo3:: Int -> Int -> Int -> Int
minimo3 x y z = min x (min y z)

dispersion::  Int -> Int -> Int -> Int
dispersion x y z = maximo3 x y z - minimo3 x y z

-- 22

celsiusToFahr:: (Fractional a) => a -> a
celsiusToFahr a = a * 1.8 + 32 

fahrToCelsius:: (Fractional a) => a -> a
fahrToCelsius a = (a - 32) / 1.8

--haceFrioF:: (Fractional a) => a -> String
--haceFrioF a | a < 8 = "Hace frio"
--            | a > 8 = "No hace frio"

-- 25 a

segundo3:: (Int, Int, Int) -> Int
segundo3 (a,b,c) = b 

ordena:: (Int,Int) ->  (Int, Int)
ordena (a,b) | a < b = (a,b)
             | a > b = (b,a)

rangoPrecioParam:: Int -> (Int,Int) -> String
rangoPrecioParam x (a, b) | x <= a = "Muy barato" 
                          | x >= b = "Demasiado caro"
                          | x > a &&  x < b = "Hay que verlo bien"

mayor3:: (Int, Int, Int) -> (Bool, Bool, Bool)
mayor3 (a,b,c) = (a>3,b>3,c>3)

todosIguales:: (Int, Int, Int) -> Bool
todosIguales (a,b,c) = a==b && b==c

esPar:: Int -> Bool
esPar x = mod x 2 == 0

soloPares:: [Int] -> [Int]
soloPares[] = [] --caso base
soloPares(x:xs) | (esPar x) = x : (soloPares xs)
                | otherwise = soloPares xs

esMayor10:: Int -> Bool
esMayor10 x = x>10

mayoresQue10:: [Int] -> [Int]
mayoresQue10[] = []
mayoresQue10(x:xs) | (x>10) = x : mayoresQue10 xs
                | otherwise = mayoresQue10 xs

mayoresQue:: Int -> [Int] -> [Int]
mayoresQue n [] = []
mayoresQue n (x:xs) | (x>n) = x : mayoresQue n xs
                    | otherwise = mayoresQue n xs

sumar1:: [Int] -> [Int]
sumar1[] = []
sumar1(x:xs) = x+1 : sumar1 xs

duplica:: [Int] -> [Int]
duplica[] = []
duplica(x:xs) = x*2 : duplica xs

todosMenores10:: [Int] -> Bool
todosMenores10 [] = True
todosMenores10(x:xs) | x < 10 = todosMenores10 xs
                     | otherwise = False

hay0:: [Int] -> Bool
hay0[] = False
hay0(x:xs) | x == 0 = True
           | otherwise = hay0 xs

suma:: [Int] -> Int
suma[] = 0
suma(x:xs) = x + suma xs

pesifica:: [Int] -> Int -> [Int]
pesifica [] n = []
pesifica(x:xs) n = x * n : pesifica xs n

cuentaVacia :: (Eq a) => [[a]] -> Int
cuentaVacia [] = 0
cuentaVacia ((x):xs) | x == [] = 1 + cuentaVacia xs
                     | otherwise = cuentaVacia xs

noPasa:: [Int] -> Int -> [Int]
noPasa[] n = []
noPasa(x:xs) n | x == n = noPasa xs n
               | otherwise =  x : noPasa xs n

ultimo:: (Eq a) => [a] -> a
ultimo[] = 0
ultimo xs | length(tail xs) == 1 = ultimo xs
        




