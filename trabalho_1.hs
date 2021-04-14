-- Exercício 1
analisa_raizes:: Int -> Int -> Int -> String
analisa_raizes a b c
                     | a == 0 = "Equacao degenerada"
                     | b * b > 4 * a * c = "Possui duas raizes reais"
                     | b * b == 4 * a * c = "Possui uma raiz real"
                     | b * b < 4 * a * c = "Nenhuma raiz real"



-- Exercício 2
equacao:: Double -> Double -> Double -> (Double, Double)
equacao a b c
     | a == 0 = ((-c / b), 0)
     | otherwise = ((-b + sqrt (b * b - 4 * a * c)) / (2 * a), (-b - sqrt (b * b - 4 * a * c)) / (2 * a))



-- Exercício 3
type Data = (Int,Int,Int)

preco_passagem:: Double -> Data -> Data -> Double
preco_passagem p (da, ma, aa) (dn, mn, an) | aa - an < 2 = p * 0.15
                                           | aa - an < 10 = p * 0.4
                                           | aa - an >= 70 = p * 0.5
                                           | otherwise = p



-- Exercício 4
-- A
gera_1:: [Int]
gera_1 = [n ^ 3 | n <- [1..20], even n, n > 3, n < 11]

-- B
gera_2:: [(Int, Int)]
gera_2 = [(x, y) | x <- [1..20], y <- [x..(x ^ 3)], x <= 5]

-- C
gera_3:: [Int]
gera_3 = [x | x <- [1..20], x < 15] ++ [x | x <- [1..20], x < 16]

-- D
gera_4:: [(Int, Int)]
gera_4 = [(n, n + 1) | n <- [1..20], n + 1 <= 10, even n]

-- E
gera_5:: [Int]
gera_5 = [x + y | (x, y) <- gera_4]



-- Exercício 5
-- A
contaNegM2:: [Int] -> Int
contaNegM2 l = length [n | n <- l, n > 0, mod n 3 == 0]

-- B
listaNegM2:: [Int] -> [Int]
listaNegM2 l = [n | n <- l, n > 0, mod n 3 == 0]



-- Exercício 6
fatores:: Int -> [Int]
fatores x = [n | n <- [1..x], mod x n == 0]

primos:: Int -> Int -> [Int]
primos x y = [n | n <- [x..y], fatores n == [1, n]]



-- Exercício 7
mdc:: Int -> Int -> Int
mdc x y | x < y = mdc y x
        | y == 0 = x
        | otherwise = mdc y (mod x y)

mmc_2:: Int -> Int -> Int
mmc_2 x y = div (x * y) (mdc x y)

mmc_3:: Int -> Int -> Int -> Int
mmc_3 x y z = mmc_2 x (mmc_2 y z)



-- Exercício 8
serie:: Float -> Int -> Float
serie x n | n == 0 = 0
          | n == 1 = x
          | odd n = ((fromIntegral n) / x) + serie x ( (fromIntegral n) - 1)
          | otherwise = (x / fromIntegral n) + serie x ( (fromIntegral n) - 1)


-- Exercício 9
fizzbuzz:: Int -> [String]
fizzbuzz x | x <= 1 = ["No"]
           | (mod x 2 == 0) && (mod x 3 == 0) = (fizzbuzz (x - 1) ++ ["FizzBuzz"])
           | mod x 2 == 0 = (fizzbuzz (x - 1) ++ ["Fizz"])
           | mod x 3 == 0 = (fizzbuzz (x - 1) ++ ["Buzz"])
           | otherwise = (fizzbuzz (x - 1) ++ ["No"])



-- Exercício 10
seleciona_multiplos:: [Int] -> Int -> [Int]
seleciona_multiplos l n = [x | x <- l, mod x n == 0]



-- Exercício 11
unica_ocorrencia:: Eq t => t -> [t] -> Bool
unica_ocorrencia x l | (length [n | n <- l, n == x]) == 1 = True
                     | otherwise = False



-- Exercício 12
intercala:: [t] -> [t] -> [t]
intercala l1 [] = l1
intercala [] l2 = l2
intercala (x1:r1) (x2:r2) = x1:x2: intercala r1 r2



-- Exercício 13
zipar:: [t] -> [t] -> [[t]]
zipar [] l2 = []
zipar l1 [] = []
zipar (x1:r1) (x2:r2) = [x1,x2]: zipar r1 r2



-- Exercício 14
type Endereco = (String, Int, String, String) -- Rua, Número, Bairro, Estado
type Contato = (String, Endereco, String, String)
type Agenda = [Contato]

c1:: Contato
c1 = ("Fabio Jose", ("Rua do Fabio", 10, "Bairro do Fabio", "Estado do Fabio"), "34991445566", "fabio@teste.com")
c2:: Contato
c2 = ("Joao Jose", ("Rua do Joao", 20, "Bairro do Joao", "Estado do Joao"), "34991556677", "joao@teste.com")
c3:: Contato
c3 = ("Pedro Jose", ("Rua do Pedro", 30, "Bairro do Pedro", "Estado do Pedro"), "34991667788", "pedro@teste.com")

agenda_pessoal:: Agenda
agenda_pessoal = [c1, c2, c3]

recupera_nome:: String -> String
recupera_nome x | [nome | (nome, end, tel, email) <- agenda_pessoal, x == email] /= [] = head [nome | (nome, end, tel, email) <- agenda_pessoal, x == email]
                | otherwise = "Email desconhecido"



-- Exercício 15
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27, 'F'),
            ("João", 1.85, 26, 'C'),
            ("Maria", 1.55, 62, 'S'),
            ("Jose", 1.78, 42, 'C'),
            ("Paulo", 1.93, 25, 'S'),
            ("Clara", 1.70, 33, 'C'),
            ("Bob", 1.45, 21, 'C'),
            ("Rosana", 1.58, 39, 'S'),
            ("Daniel", 1.74, 72, 'S'),
            ("Jocileide", 1.69, 18, 'S') ]

-- A
altura_media:: Float
altura_media = (sum [altura | (_, altura, _, _) <- pessoas]) / fromIntegral (length pessoas)

-- B
mais_nova:: Int
mais_nova = minimum [idade | (nome, altura, idade, estado) <- pessoas]

-- C
mais_velha:: (String, Char)
mais_velha = head [(nome, estado) | (nome, altura, idade, estado) <- pessoas, idade == maximum [idade | (nome, altura, idade, estado) <- pessoas]]

-- D
dados_50:: [Pessoa]
dados_50 = [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- pessoas, idade > 50]

-- E
casadas:: Int -> Int
casadas i = length [(nome, altura, idade, estado) | (nome, altura, idade, estado) <- pessoas, estado == 'C', idade > i]



-- Exercício 16
insere_ord:: (Ord t) => t -> [t] -> [t]
insere_ord n [] = [n]
insere_ord n (x:r) | n <= x = (n:x:r)
                   | otherwise = x: insere_ord n r



-- Exercício 17
reverte:: [t] -> [t]
reverte [] = []
reverte (x:r) = reverte r ++ [x]



-- Exercício 18
pertence:: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
                               else pertence a z

elimina_repet:: Eq t => [t] -> [t]
elimina_repet [] = []
elimina_repet [x] = [x]
elimina_repet (x:r) | pertence x r = elimina_repet r
                    | otherwise = x: elimina_repet r

