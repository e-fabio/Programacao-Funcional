-- Exercício 2
-- A) [5,4..1]
-- B) ['a','c'..'e']
-- C) [1,4..16]
-- D) zip [1,-2..(-11)] [1,5..17]


-- Exercício 3
-- A
intervalo::Int->Int->[Int]
intervalo a b
   | a > b = []
   | a == b = [a]
   | otherwise = [a..b]

-- B
intervaloPar::Int->Int->[Int]
intervaloPar a b
   | a >= b = []
   | even a = a:intervaloPar (a + 1) b
   | otherwise = intervaloPar (a + 1) b


-- Exercício 5
intervaloQuadrado::Int->Int->[Int]
intervaloQuadrado a b
   | a >= b = []
   | otherwise = [a * a | a <- [a..b]]


-- Exercício 6
selecionaImpares::[Int]->[Int]
selecionaImpares (x:y:r)
   | (x:y:r) == [] = []
   | otherwise = [x | x <- (x:y:r), odd x]