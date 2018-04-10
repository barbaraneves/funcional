-- Capítulo 2: Matemática Funcional --

-- Questão 1 --
iguais_3 a b c | (a /= b) && (a /= c) && (b /= c) = 0 -- Condição 1: São todos diferentes.
               | a == b && b == c = 3 -- Condição 2: São todos iguais.
               | otherwise = 2 -- Condição 3: Se não foram todos iguais e diferentes, logo, tem dois iguais e um diferente. 
--


-- Questão 2 --
soma_2 a b = a + b -- Função que soma dois números a e b.
soma_3 a b c = soma_2 c (soma_2 a b) -- Função que soma três números a, b e c.
media_3 a b c = (soma_3 a b c)/3 -- Função que tira a média de 3 números. 
comparando a b c | (a > media_3 a b c) && (b > media_3 a b c) && (c > media_3 a b c) = 3
                 | (a <= media_3 a b c) && (b <= media_3 a b c) && (c <= media_3 a b c) = 0  
                 | (a > media_3 a b c) && (b > media_3 a b c) = 2
                 | (a > media_3 a b c) && (c > media_3 a b c) = 2
                 | (b > media_3 a b c) && (c > media_3 a b c) = 2
                 | otherwise = 1  
--

 
-- Questão 3 --
potencia_2 n = n^2
--


-- Questão 4 --
potencia_4 n = (potencia_2 n)^2
--

-- Questão 5 --
or_exclusivo a b = (a || b) && (not (a && b))

-- Questão 6 --
delta a b c = potencia_2 b - (4 * a * c)
raiz_delta a b c = sqrt(delta a b c)
x_maior a b c = ((- b) + raiz_delta a b c) / (2 * a)
x_menor a b c = ((- b) - raiz_delta a b c) / (2 * a)
--

