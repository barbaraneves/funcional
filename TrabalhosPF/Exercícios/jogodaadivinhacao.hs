-- Questão bônus para a AP2 --

{- Aluna: Bárbara Stéphanie
   Matrícula: 388713 -}

import System.Random 

{- Função auxiliar que compara o número gerado aleatoriamente 
   com o chute que foi dado, até todas as chances acabarem -}
compara :: Int -> Int -> IO ()

compara _ 0 = do
	putStrLn ("Game Over!")
compara numero chances = do
	putStrLn(" ")
	putStrLn ("Voce tem " ++ show chances ++ " chances para adivinhar o numero.")
	putStr ("Informe um numero: ") 
	valor <- getLine 
	putStrLn (" ")
	let chute = read valor :: Int

	if chute == numero 
		then putStrLn ("Parabens, voce acertou!")
	else do
		putStrLn ("Errou!")
		compara numero (chances - 1)


{- Função que era o número aleatório e 
   utiliza a que compara os números -}
jogodaadivinhacao numero chances = do 
	gen <- getStdGen
	let (numAleatorio, x) = randomR (1 :: Int, numero) gen
	compara numAleatorio chances