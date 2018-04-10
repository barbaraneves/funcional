-- Conteúdos da API -- 

-- Capítulo 5: Listas --
{- função que encontra por quantos números o número passado como parâmetro é divisível -}
n_divisiveis :: Int -> Int -> Int 
n_divisiveis a 1 = 1
n_divisiveis a b | ((mod a b) == 0 = 1 + (n_divisiveis a (b-1))
                 | otherwise = (n_divisiveis a (b-1))

{- função para verificar se um número é primo -}
eh_primo :: Int -> Bool
eh_primo 0 = False
eh_primo 1 = True
eh_primo n | ((n_divisiveis n n) == 2) = True
           | otherwise = False

{- função que encontra o primeiro número primo na lista -}
primeiro_primo :: [Int] -> Int
primeiro_primo (x:y) | (eh_primo x) = x
                     | otherwise = (primeiro_primo y)

{- função que concatena duas listas -}
concatena [] a = a
concatena (x:y) a = x:concatena y a

{- função que inverte uma lista -}
inverte [] = []
inverte (x:y) = (concatena (inverte y) [x])

{- função que devolve o menor valor de uma lista -}
menor [a] = a
menor (a:x) | a < (menor x) = a  -- o a é o menor de todos
            | otherwise = (menor x)  -- o menor está no corpo da lista

{- função que retorna uma lista sem o menor valor da lista -}
remove_menor [a] = []
remove_menor (a:x) | a == (menor (a:x)) = x  -- a cabeça é o menor, então remova-o
                   | otherwise = a : (remove_menor x)

{- função que ordena uma lista -}
ordena_lista [] = []
ordena-lista [a] = [a]
ordena_lista 1 = (menor 1) : (ordena_lista (remove_menor 1))
{- A ideia da implementação acima é a seguinte: uma nova lista ordenada 1 tem como cabeça o menor elemento de 1, e como corpo uma lista ordenada, sem o menor elemento de 1 -}

{- função que agrupa duas listas -}
zipar (a:b) (x:y) = (a, x) : (zipar b y)
zipar _ _ = []

{- função que adiciona um objeto na lista -}
insere a [] = [a]
insere a (x:y) | a == x = x:a
               | otherwise = x : (insere a y)

{- função que valida uma estrutura do tipo lista -}
lista [] = True
lista (a:x) = lista x 

{- função que indica o índice de um elemento da lista -}
indice a [] = -1
indice _ [_] = 0
indice a (x:y) | x == a = 0
               | otherwise = (indice a y) + 1

{- questão 3 do livro -}
data_lista = [("João", 21), ("Alex", 32), ("Aloisio", 12)]

pega_idade (_, x) = x
pega_nome (x, _) = x

menor [a] = pega_nome a
menor (a:b:x) | pega_idade (a) <= pega_idade (b) = menor (a:x)
              | otherwise = menor (b:x)

maior [a] = pega_nome a
maior (a:b:x) | pega_idade (a) >= pega_idade (b) = maior (a:x)
              | otherwise = maior (b:x)
