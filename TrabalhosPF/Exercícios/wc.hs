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

    