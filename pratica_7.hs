-- Exercício 1
paridade:: [Int] -> [Bool]
paridade l = map even l


-- Exercício 2
prefixos:: [String] -> [String]
prefixos l = map (take 3) l


-- Exercício 3
saudacao:: [String] -> [String]
saudacao l = map ("Oi " ++) l


-- Exercício 4
filtrar:: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (x:xs) | f x = x : filtrar f xs
                 | otherwise = filtrar f xs

filtrar2:: (a -> Bool) -> [a] -> [a]
filtrar2 f [] = []
filtrar2 f (x:xs) = [ x | x <- (x:xs), f x]


-- Exercício 5
pares:: [Int] -> [Int]
pares lst = filter even lst


-- Exercício 6
solucoes:: [Int] -> [Int]
solucoes l = filter (\x -> (5 * x + 6) < (x * x)) l


-- Exercício 7
maior:: [Int] -> Int
maior l = foldr1 max l


-- Exercício 8
menor_min10:: [Int] -> Int
menor_min10 l = foldr min 10 l


-- Exercício 9
junta_silabasplural:: [String] -> String
junta_silabasplural silabas = foldr (++) "s" silabas


-- Exercício 10
menores10:: [Int] -> ([Int], Int)
menores10 l = (filter (\x -> x < 10) l, length (filter (\x -> x < 10) l))


-- Exercício 11
busca::(Eq a) => a -> [a] -> (Bool, Int)
busca elem [] = (False, 0)
busca elem (x:xs) | elem == x = (True, 1)
                  | otherwise = busca elem xs

