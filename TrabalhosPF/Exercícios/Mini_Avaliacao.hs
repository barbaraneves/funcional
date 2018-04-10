-- Questões da Mini-Avaliação de Funcional --

{- Aluna: Bárbara Stéphanie	
   Matrícula: 388713 -}


-- Lista de Exercícios 02 --

{- 5.  
    a) reflexiva -}
reflexiva::[Integer] -> [(Integer, Integer)] -> Bool
reflexiva list rel = and [elem (x, x) rel | x <- list]

{- 5. 
   b) simetrica -}
simetrica::[(Integer, Integer)] -> Bool
simetrica rel = and [elem (snd(x), fst(x)) rel | x <- rel]

{- 5. 
   c) transitiva -}
aux (x:xs) = xs

transitiva :: [(Integer, Integer)] -> Bool
transitiva [] = True
transitiva rel = and [elem (fst(x), snd(y)) rel | x <- rel, y <- aux(rel)]


-- Lista de Exercícios 03 --

{- 4. 
   (Não consegui fazer.) -}


-- Lista de Exercícios 04 --

{- 16. -}
horner::[Double] -> Double -> Double

	{- Forma recursiva:

	   horner [] z = 0
	   horner (c:cs) z = c + z * (horner cs z) 
	-}

-- Usando a função de ordem superior foldr --
horner cs z = foldr(\x acc -> x + z * acc) 0 cs
