data Color = Rojo | Amarillo | Azul | Verde
    deriving (Show, Eq)

data Forma = Triangulo | Cuadrado | Rombo | Circulo
    deriving (Show, Eq)

type Figura = (Forma, Color, Int)

rojo :: Figura -> Bool
rojo (f,c,t) = c == Rojo

azul :: Figura -> Bool
azul (f,c,t) = c == Azul

amarillo :: Figura -> Bool
amarillo (f,c,t) = c == Amarillo 

verde :: Figura -> Bool
verde (f,c,t) = c == Verde

circulo :: Figura -> Bool
circulo (f,c,t) = f == Circulo

rombo :: Figura -> Bool
rombo (f,c,t) = f == Rombo

cuadrado :: Figura -> Bool
cuadrado (f,c,t) = f == Cuadrado 

triangulo :: Figura -> Bool
triangulo (f,c,t) = f == Triangulo 

tam :: Figura -> Int
tam (f,c,t) = t 

-- a) Todas las figuras de xs son rojas

figrojas :: [Figura] -> Bool
figrojas [] = True
figrojas (x:xs) | (rojo x == True) = figrojas xs
                | otherwise = False

-- b) Todas las figuras de xs son de tamaño menor a 5

menorque5 :: [Figura] -> Bool
menorque5 [] = True
menorque5 (x:xs) | (tam x<5) = menorque5 xs
                 | (tam x>5) = False

-- c) Todos los triangulos de xs son rojos

trianrojo :: [Figura] -> Bool
trianrojo [] = True
trianrojo (x:xs) | (triangulo x && rojo x) = trianrojo xs
                 | otherwise = False

-- d) Existe un cuadrado verde en xs

cuaverde :: [Figura] -> Bool
cuaverde [] = False 
cuaverde (x:xs) | (cuadrado x && verde x) = True
                | (cuadrado x && verde x) == False = cuaverde xs

-- e) Todos los circulos de xs son azules y de tamaño menor a 10

cirazul10 :: [Figura] -> Bool
cirazul10 [] = True
cirazul10 (x:xs) | (circulo x && azul x && tam x<10) = cirazul10 xs
                 | (circulo x && azul x && tam x>10) = False
                 | otherwise = False

-- f) Ningun triangulo de xs es azul

trianoazul :: [Figura] -> Bool
trianoazul [] = True
trianoazul (x:xs) | (triangulo x && azul x == False) = trianoazul xs
                  | otherwise = False

-- g) En xs no hay circulos amarillos ni verdes 

cirnovernoama :: [Figura] -> Bool
cirnovernoama [] = True 
cirnovernoama (x:xs) | (circulo x && (amarillo x || verde x) == True) = False
                     | (circulo x && (amarillo x || verde x) == False) = cirnovernoama xs
                     
-- h) Existe (al menos) un cuadrado de tamaño menor a 5 en xs

cuamen5 :: [Figura] -> Bool
cuamen5 [] = False
cuamen5 (x:xs) | (cuadrado x && tam x<5) = True
               | otherwise = cuamen5 xs

-- i) Si hay circulos rojos en xs entonces hay cuadrados rojos

ciruja :: [Figura] -> Bool
ciruja [] = False
ciruja (x:xs) | (circulo x && rojo x) = True && ciruja xs
              | (cuadrado x && rojo x) = True
              | (cuadrado x && rojo x == False) = ciruja xs
              | (circulo x && rojo x == False) = True
              | otherwise = ciruja xs


-- fijate si lo podes simplificar jaja