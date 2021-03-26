-- Exercício 1
-- A
or_1::Bool->Bool->Bool
or_1 True x = True
or_1 x True = True
or_1 x y = False

or_2::Bool->Bool->Bool->Bool
or_2 True x y = True
or_2 x True y = True
or_2 x y True = True
or_2 x y z = False

or_3::Bool->Bool->Bool->Bool->Bool
or_3 True x y z = True
or_3 x True y z = True
or_3 x y True z = True
or_3 x y z True = True
or_3 x y z w = False

-- B
or_4::Bool->Bool->Bool
or_4 x y
   | x == True = True
   | y == True = True
   | otherwise = False
   
or_5::Bool->Bool->Bool->Bool
or_5 x y z
   | x == True = True
   | y == True = True
   | z == True = True
   | otherwise = False


-- Exercício 2
distancia::(Float,Float)->(Float,Float)->Float
distancia (x1,y1) (x2,y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)


-- Exercício 3
fatorial_1::Int->Int
fatorial_1 x
   | x == 0 = 1
   | otherwise = x * fatorial_1 (x - 1)

fatorial_2::Int->Int
fatorial_2 0 = 1
fatorial_2 x = x * fatorial_2 (x - 1)


-- Exercício 4
fibo::Int->Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x - 2) + fibo(x - 1)


-- Exercício 5
n_tri::Int->Int
n_tri 0 = 0
n_tri 1 = 1
n_tri x = x + n_tri (x - 1)


-- Exercício 6
potencia2::Int->Int
potencia2 0 = 1
potencia2 x = 2 * potencia2 (x - 1)


-- Exercício 7
-- A
prodIntervalo::(Int,Int)->Int
prodIntervalo (m,n)
   | m > n = 0
   | m == n = m
   | m < n = n * prodIntervalo (m,(n - 1))

-- B
fatorial_3::Int->Int
fatorial_3 x 
   | x == 0 = 1
   | otherwise = prodIntervalo (1,x)

