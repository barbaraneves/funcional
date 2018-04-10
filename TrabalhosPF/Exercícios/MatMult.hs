import System.Environment
import Lista_de_Exercicios_III.hs


main = do
       params <- getArgs
       let tamanho = read (params !! 0)::Int
           matriz_A = umMatriz tamanho tamanho
           matriz_B = umMatriz tamanho tamanho
           matriz_C = produtoMatriz matriz_A matriz_B 
--       putStrLn $ "Matriz C: " ++ show (matriz_C)
       return ()