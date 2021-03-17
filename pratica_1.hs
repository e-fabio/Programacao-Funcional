dobro::Int->Int
dobro x = x * 2

quadruplo::Int->Int
quadruplo x = dobro (dobro x)

hipotenusa x y = sqrt(x^2 + y^2)

distancia x1 y1 x2 y2 = sqrt((x2 - x1)^2 + (y2 - y1)^2)