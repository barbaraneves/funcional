-- Questão 2 da Lista 5: Código executável --

import System.Environment

main = do 
    n <- getArgs
    let qtd = read (head n) :: Int
    mapM putStrLn (map (\x -> "Se " ++ show x ++ " elefantes incomodam muita gente, \n" ++ show (x + 1) ++ " elefantes incomodam muito mais!") [2..(qtd - 1)])