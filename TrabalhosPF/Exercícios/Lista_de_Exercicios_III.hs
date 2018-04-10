-- Programação Funcional: Lista de Exercícios III --

-- Assunto: Tipos Abstratos de Dados --

{- 1. -}
data Fracao = F Int Int
Instance Num Fracao
Instance Nq Fracao 
Instance Show Fracao

{- 2. -}
module Stack
(
      push, 
      pop, 
      height, 
      top, 
      empty,
      isEmpty
) where

data Stack a = Empty | Top a (Stack a) deriving (Show)

push :: a -> Stack a -> Stack a

pop :: Stack a -> Maybe Stack a

height :: Stack a -> Int

top :: Stack a -> Maybe a 

empty :: Stack a 

isEmpty :: Stack a

{- 3. -}
module
(
      startQueue,
      endQueue, 
      pushQueue, 
      popQueue,
      isEmptyQueue,
      lenQueue,
      whileNotEmpty
) where

      data Queue a = Empty | Start a (Queue a) deriving (Show)

      startQueue :: Queue a -> Maybe a
      endQueue :: Queue a -> Maybe a 
      pushQueue :: a -> Queue a -> Queue a 
      popQueue :: Queue a -> Queue a 
      isEmptyQueue :: Queue a -> Bool
      lenQueue :: Queue a -> Int
      whileNotEmpty :: (a -> b) -> Queue a -> [b]


{- 4. -}
module Matriz
(  zeroMatriz,
   umMatriz,
   matrizIdentidade,
   somaMatriz,
   produtoEscalar,
   produtoMatriz,
   listaParaMatriz
) where

type Linha = [Float]
data Matriz = Matriz { nlinhas :: Int,
                       ncolunas :: Int, 
                       linhas :: [Linha] } deriving (Show)

zeroMatriz :: Int -> Int -> Matriz
zeroMatriz m n = Matriz m n matriz_zero 
                 where matriz_zero = [ linha_zero | a <- [1..m]]
                       linha_zero = [ 0 | a <- [1..n]] 

umMatriz :: Int -> Int -> Matriz
umMatriz m n = Matriz m n matriz_um 
                 where matriz_um = [ linha_um | a <- [1..m]]
                       linha_um = [ 1 | a <- [1..n]] 

matrizIdentidade :: Int -> Matriz
matrizIdentidade n = Matriz n n identidade
                     where identidade = [ linha_zero i | i <- [1..n]]
                           linha_zero i = map (\x -> if x then 1 else 0) $ pos i n
                           pos i n = [ x == i | x <- [1..n] ]

somaMatriz :: Matriz -> Matriz -> Matriz
somaMatriz (Matriz la ca a) (Matriz lb cb b) | la /= lb || ca /= cb = zeroMatriz 0 0
                                             | otherwise = Matriz la ca (soma a b)
                                             where soma a b = zipWith (soma_linha) a b
                                                   soma_linha p q = zipWith (+) p q  
produtoEscalar :: Float -> Matriz -> Matriz
produtoEscalar escalar (Matriz linhas colunas dados) = Matriz linhas colunas novosDados
                                                       where novosDados = [ produtoLinha linha escalar | linha <- dados ]
                                                             produtoLinha linha escalar = [ item * escalar | item <- linha ]

produtoMatriz :: Matriz -> Matriz -> Matriz
produtoMatriz (Matriz linhas_a colunas_a dados_a) 
              (Matriz linhas_b colunas_b dados_b) | colunas_a /= linhas_b = zeroMatriz 0 0 
                                                  | otherwise = Matriz linhas_a colunas_b novosDados
                                                   where novosDados = [ novaLinha i | i <-[0..(linhas_a - 1)] ]
                                                         novaLinha i = [ elemento i j | j <- [0..(colunas_b - 1)] ]
                                                         elemento i j = foldl1 (+) $ zipWith (*) (linha i dados_a) (coluna j dados_b)
                                                         linha i matriz = matriz !! i
                                                         coluna j matriz = map (!! j) matriz

listaParaMatriz :: [Linha] -> Matriz
listaParaMatriz lista = Matriz l c lista 
                        where l = length lista
                              c = length (lista !! 0) 