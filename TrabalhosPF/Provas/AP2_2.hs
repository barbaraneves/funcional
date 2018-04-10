-- Questões da APII de Funcional: Parte 1 --

{- Aluna: Bárbara Stéphanie Neves
   Matrícula: 388713 -}

{- Questão 3, (a) -}
insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a]
insert a lista | a > (head lista) = (head lista) : (insert a (tail lista)) 
               | otherwise = a : lista

{- Questão 3, (b) -}
insertSort :: Ord a => [a] -> [a]
insertSort [a] = insert a []
insertSort lista = insert (head lista) (insertSort (tail lista))

{- Questão 3, (c) -}
insertSortFold :: Ord a => [a] -> [a]
insertSortFold lista = foldr (\a -> insert a) [] lista

{- Questão 4, (a) -}
main = interact wc

wc input = 
    let linhas = "Linhas: " ++ (show (length (lines input))) ++ "."
        palavras = "Palavras: " ++ (show (length (separador input))) ++ "."
        chars = "Caracteres: " ++ (show (length input)) ++ "."
        result = linhas ++ "\n" ++ palavras ++ "\n" ++ chars
    in  result

separa [] = []
separa texto = (takeWhile (/= ' ') texto) : separa (dropWhile (== ' ') (dropWhile (/= ' ') texto))

{- Questão 4, (b) -}
fatorial = do
   putStrLn "Insira o valor de n: "
   n <- getLine
   let valor = read n :: Int
   putStrLn ("O fatorial de " ++ show valor ++ " eh " ++ (show (foldl (*) 1 [2..valor])) ++ ".")

{- Questão 4, (c) -}
primo = do 
    putStrLn "Verifique se n é primo ou não.\nInsira o valor de n: "
    n <- getLine
    let valor = read n :: Int 
    if (valor <= 1) || ((length (filter (\x -> (mod valor x) == 0) [2..(valor - 1)])) > 0)
        then putStrLn "Nao." 
    else putStrLn "Sim."