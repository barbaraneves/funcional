elefantes :: IO ()
elefantes = do
        n <- getLine
        let qtd = read n :: Int
        mapM_ putStrLn (map (\x -> "Se " ++ show x ++ " Elefantes incomentao muita gente \n" ++ show (x+1) ++ " elefantes incomodam muito mais") [2..(qtd - 1)] )

--questao-03
--module Main where
    --main = do{
         --test <- getLine
         --contents <- getContents
         --putStrLn $ show $ take (read test) (line contents)
        -- }

--Questão 4 -- Esta deve obter a entrada a partir de um arquivo txt... no terminal deve ta escrito mais ou menos assim cat texto.txt | ./questão4 
-----------------------------------
main = interact revertLines

revertLines input =
    let allLines = lines input
        inverseLines = map reverse allLines
        result = unlines inverseLines
    in result
---------------------------------- As duas fazem as mesmas coisas só que uma ta com paquita
main = interact $ unlines . map reverse . lines

--Questão 5 -- Recebe um arquivo e retorna a quantidade de linhas, palavras e caracteres
------------------------------------------------------------------------------------------------
main :: IO ()
main = commandLineUtility (format . cwlcount)

commandLineUtility :: (String -> String) -> IO ()
commandLineUtility fn = do
       putStrLn "Please enter the filename:"
       filename <- getLine
       putStrLn ("The file name you have entered is: " ++ filename)
       contents <- readFile filename -- read the file specified in “name” into “contents”
       lower <- (return . fn) contents
       putStrLn lower 

cwlcount :: String -> (Int, Int, Int)
cwlcount str = (length str, length (words str), length (lines str))

format :: (Int, Int, Int) -> String
format (c,w,l) = unlines $
     ["Number of characters:"
     , show c
     , "Number of words:"
     , show w
     , "Number of lines:"
     , show l
     ]

--(Mas como foi o Clayton que colocou vamos ignorar ;-)  )
---------------------------------------------------------------------------------------------
main = interact wc

wc input = 
    let liness = "Linhas: " ++ (show(length (lines input))) ++ "."
        words = "Palavras: " ++ (show(length (separador input))) ++ "."
        characters = "Caracteres: " ++ (show(length input)) ++ "."
        result = liness ++ "\n" ++ words ++ "\n" ++ characters
    in  result

separador [] = []
separador texto = (takeWhile (/= ' ') texto) : separador (dropWhile (== ' ') (dropWhile (/= ' ') texto))

--questao 06
fatorial :: IO ()
fatorial = do
      n <- getLine
      let fat = read n :: Int
      putStrLn (show (foldl (*) 1 [2..fat]))

--Questao 07
primo :: IO ()
primo = do
    n <- getLine
    let prim = read n :: Int
    if length (filter (\x -> (mod prim x) == 0) [2..(floor (sqrt(fromIntegral prim)))]) > 0
       then putStrLn "Nao"
    else putStrLn "Sim"

--Questao 09
somaAculumador :: IO ()
somaAculumador = do
           n <- getLine
           let num = read n :: Int 
           x <- sequence (replicate num getLine)
           let xs = read x :: [Int]
           putStrLn (show(sum xs))
