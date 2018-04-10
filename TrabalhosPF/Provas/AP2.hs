-- Questões da APII de Funcional: Parte 1 --

{- Aluna: Bárbara Stéphanie Neves
   Matrícula: 388713 -}

{- Questão 1 -}
module Polinomio
(
    polinomioZero,
    ePolinomioZero,
    construirPolinomio,
    grau,
    coeficienteLider,
    restoDoPolinomio
) where
   
    data Polinomio a = PolinomioZero 
                     | ConstruirPolinomio Int a (Polinomio a) deriving Eq
    
    polinomioZero :: Polinomio a
    polinomioZero = PolinomioZero            
    
    ePolinomioZero :: Polinomio a -> Bool
    ePolinomioZero PolinomioZero = True
    ePolinomioZero _ = False
                             
    construirPolinomio :: Num a => Int -> a -> Polinomio a -> Polinomio a
    -- Onde: n é o expoente, b é o coeficiente e p é um polinômio
    -- Forma: b * x ^ n + p
    --construirPolinomio _ 0 p = p
    --construirPolinomio n b PolinomioZero = ConstruirPolinomio n b PolinomioZero
    
    grau :: Polinomio a -> Int
    grau PolinomioZero = 0
    grau (ConstruirPolinomio n _ _) = n
    
    coeficienteLider :: Num a => Polinomio a -> a
    coeficienteLider PolinomioZero = 0
    coeficienteLider (ConstruirPolinomio _ b _) = b
    
    restoDoPolinomio :: Polinomio a -> Polinomio a
    restoDoPolinomio PolinomioZero = PolinomioZero
    restoDoPolinomio (ConstruirPolinomio _ _ p) = p
    
    instance (Show a, Eq a) => Show (Polinomio a) where


{- Questão 2 -}
type Vertice = Int 
type Grafo = ([Vertice], [(Vertice, Vertice)])

caminho :: Grafo -> [Vertice] -> Bool
caminho (a, b) c = all (`elem` b) (arestas c)

arestas l | length (tail l) < 1 = []
          | otherwise = (head l, (head (tail l))) : arestas (tail l)

--vertices l _ = False
--vertices l n | (head l) == n = True
--             | otherwise = vertices (tail) n

