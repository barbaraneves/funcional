-- Questões da API de Funcional --

{- Aluna: Bárbara Stéphanie Neves
   Matrícula: 388713 -}

{- 1. isPalind -}
isPalindAux [] = []
isPalindAux (x:y) = (isPalindAux y) ++ [x] 

isPalind y | y == (isPalindAux y) = True
           | otherwise = False

{- 2. rotDir -}
corpo [_] = [] -- função auxiliar que retorna uma lista que não possui o último elemento.
corpo (a:x) = a : (corpo x)

rotDir n y | n < 1 = y
           | otherwise = (rotDir (n - 1)) ((last y) : (corpo y)) 
             -- a função "last" usada já é do Haskell e retorna o último elemento da lista.


{- 3. splitints -}

-- As funções abaixo são auxiliares que identificam se um número é par ou ímpar:
par a | a == 0 = True
      | a >= 2 && (mod a 2) == 0 = True
      | otherwise = False

impar b | b == 1 = True
        | par b /= True = True
        | otherwise = False 

splintintsAuxPar [] = []
splintintsAuxPar (a:x) | par a == True = (splintintsAuxPar x) ++ [a]
 
splintintsAuxImpar [] = []
splintintsAuxImpar (b:y) | impar b == True = (splintintsAuxImpar y) ++ [b]

splintints (a:x) = ((splintintsAuxPar x) ++ [a], (splintintsAuxImpar x) ++ [a])

{- 5. intercal -}
intercal x y = y ++ x

