-- Programação Funcional: Listas de Exercícios I --

{- 1. menorDeDois -}
menorDeDois :: Int -> Int -> Int 
menorDeDois x y | x < y = x
                | otherwise = y

{- 2. menorDeTres -}
menorDeTres :: Int -> Int -> Int -> Int
menorDeTres x y z | (menorDeDois x y) < z = (menorDeDois x y)
                  | (menorDeDois y z) < x = (menorDeDois y z)
                  | otherwise = (menorDeDois x z)

{- 3. fatorial -}
fatorial 0 = 1
fatorial n = fatorial (n - 1) * n

{- 4. fibonacci -}
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{- 5. elemento -}
elemento a [] = -1 -- Atribuí o valor de -1 caso a lista seja vazia ou o elemento seja maior que o tamanho da lista. 
elemento a (x:y) | a == 0 = x
                 | otherwise = (elemento (a - 1) y)

{- 6. pertence -}
pertence a [] = False
pertence a (x:y) | x == a = True
                 | otherwise = (pertence a y) 

{- 7. total -}
total [] = 0
total (a:x) = 1 + (total x) 

{- 8. maior -}
maior [a] = a
maior (a:x) | a > (maior x) = a
            | otherwise = (maior x)

{- 9. frequencia -}
frequencia a [] = 0
frequencia a (x:y) | x == a = 1 + (frequencia a y) 
                   | otherwise =  (frequencia a y)  

{- 10. unico -}
unico a [] = False
unico a (x:y) | (frequencia a (x:y)) /= 1 = False
              | otherwise = True

{- 11. maioresQue -}
maioresQue a [] = []
maioresQue a (x:y) | x > a = (x : (maioresQue a y))
                   | otherwise = (maioresQue a y)

{- 12. concatena -}
concatena :: [Int] -> [Int] -> [Int]
concatena [] a = a
concatena (x:y) a = x : (concatena y a) 

{- 13. calda -}
calda [] = []
calda (a:x) = x

{-14. corpo -}
corpo [a] = []
corpo (a:x) = a : corpo x

{- 15. unique -}
unique [] = []
unique (a:x) | frequencia a (a:x) == 1 = a : (unique x)
             | otherwise = (unique x)

{- 16. menores -}
menores a [] = []
menores a (x:y) | x <= a = x : (menores a y)
                | otherwise = (menores a y)

{- 17. alter -}
alter_aux n x | x > n = []
              | otherwise = x : (x*(-1)) : (alter_aux n (x + 1))

alter n = alter_aux n 1

{- 18. reverso -}
 
