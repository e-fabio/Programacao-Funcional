-- Exercício 1
dobro::Int->Int
dobro x = x * 2

quadruplo::Int->Int
quadruplo x = dobro (dobro x)

hipotenusa::Float->Float->Float
hipotenusa x y = sqrt(x^2 + y^2)

distancia::Float->Float->Float->Float->Float
distancia x1 y1 x2 y2 = sqrt((x2 - x1)^2 + (y2 - y1)^2)

-- Exercício 3
conversao::Float->(Float,Float,Float)
conversao x = (x, x * 3.96, x * 4.45)

-- Exercício 4
bissexto::Int->Bool
bissexto x | (mod x 400 == 0) = True
           | (mod x 4 == 0) && (mod x 100 /= 0) = True
           | otherwise = False


-- Exercício 5
type Data = (Int,Int,Int)
bissexto2::Data->Bool
bissexto2 (x,y,z) = if bissexto z then True
                                  else False

-- Exercício 6
valida::Data->Bool
valida (d,m,a)
     | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
     | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
     | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
     | d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
     | otherwise = False

-- Exercício 7
precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) = valida (d1,m1,a1) && valida (d2,m2,a2) &&
                                ((a1 < a2) ||
                                 (a1 == a2 && m1 < m2) ||
                                 (a1 == a2 && m1 == m2 && d1 < d2)) 

-- Exercício 8
type Livro = (String,String,String,String,Int)
type Aluno = (String,String,String,String)
type Emprestimo = (String,String,Data,Data,String)

-- Exercício 9
e1:: Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emprestimoValido::Emprestimo->Data->Bool
emprestimoValido (_,_,inicio,fim,_) hoje = (precede hoje fim) && (not (precede hoje inicio))

-- testes
-- emprestimoValido e1 (19,9,2009)
-- emprestimoValido e1 (25,9,2009)
-- emprestimoValido e1 (10,9,2009)
