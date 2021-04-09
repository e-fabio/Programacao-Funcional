-- Exercício 1
conta_ch:: [Char] -> Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

conta:: [t] -> Int
conta [] = 0
conta (x:r) = 1 + conta r

maior:: [Int] -> Int
maior [x] = x
maior (x:y:resto)
   | x > y = maior (x:resto)
   | otherwise = maior (y:resto)

primeiros:: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n - 1) xs

pertence:: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
                               else pertence a z

uniaoR:: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if (pertence x l) then uniaoR xs l
                                  else x: uniaoR xs l


-- Exercício 2
npares:: [Int] -> Int
npares [] = 0
npares (x:r) = if (even x) then 1 + npares r
                           else npares r


-- Exercício 3
produtorio:: Num t => [t] -> t
produtorio [] = 0
produtorio [x] = x
produtorio (x:r) = x * produtorio r


-- Exercício 4
comprime:: [[Int]] -> [Int]
comprime [] = []
comprime [[]] = []
comprime [[x]] = [x]
comprime [(x:r)] = x: comprime [r]
comprime ([]:l) = comprime l
comprime ((x:r):l) = x: (comprime (r:l))


-- Exercício 5
tamanho:: [t] -> Int
tamanho [] = 0
tamanho (x:r) = 1 + tamanho r


-- Exercício 6
-- uniaoRec2:: [Int] -> [Int] -> [Int]
-- uniaoRec2 [] _ = _
-- uniaoRec2 _ [] = _
-- uniaoRec2 [] [] = []

