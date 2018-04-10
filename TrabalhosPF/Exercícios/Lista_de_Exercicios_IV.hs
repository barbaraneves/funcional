	--lista 4
--1.
menorque' (x:xs) = [x < media (x:xs) | x <- (x:xs)]

menorque (x:xs) = filter (<media (x:xs)) (x:xs)
maiorque (x:xs) = filter (>media (x:xs)) (x:xs)

divideMedia :: [Double] -> ([Double], [Double])
divideMedia x = (menorque x, (maiorque x))

media' :: [Double] -> Double
media' [] = 0
media' (x:xs) = (x + media' xs)

media (x:xs) = media' (x:xs)/ fromIntegral (length (x:xs))


--2.
fatAcc x = scanl (*) 1 [2..x]


--3.
partir :: Int -> [a] -> [[a]]
partir _ [] = []
partir x (y:ys) = (take x (y:ys)):(partir x (drop x (y:ys)))

--4.
--group :: [Int] -> [(Int, Int)]
--conta (x:xs) = takeWhile (/=x) (x:xs)

--6.
relacionados:: (Int->Int->Bool) -> [Int] -> Bool
relacionados f [] = True
relacionados f [a] = True
relacionados f (x:xs:xss) | (f x xs) = True && (relacionados f (xs:xss)) | otherwise = False

--7.
--agrupa :: Eq a => [[a]] -> [[a]]
--agrupa (x:xs)

--8.
mystery xs = foldr (++) [] (map sing xs)
sing x = [x]

--9.

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

concatenaFold :: [[a]] -> [a]
concatenaFold [] = []
concatenaFold (x:xs) = foldl (\acc a -> acc ++ a) x xs

--10.
inverteFold :: [a] -> [a]
inverteFold [] = []
inverteFold xs = foldl (\acc a -> acc ++ a) [last xs] [init xs]

--11.
tamanhoFold :: [a] -> Int
tamanhoFold [] = 0
tamanhoFold (x:xs) = foldl (\acc a -> acc + 1) 1 xs

--12.
--elementoFold :: [a] -> Bool
--elementoFold y [] = False
--elementoFold y xs  foldr (\a acc -> acc == a) y xs

--13.
--a. funcao recursiva
paridade :: [Bool] -> Bool
paridade (x:xs) = odd (paridade' (x:xs))

paridade' [] = 0
paridade' (x:xs) | x == True = 1 + paridade' xs | otherwise = 0 + paridade' xs

--b. com foldr

--14.

--15.
--a. compreensao de lista
filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica f p xs = [f x | x <- xs, p x]

--b. map e filter
filtraAplica1 f p xs = map f (filter p xs)

--c. recursao
filtraAplica2 f p [] = []
filtraAplica2 f p (x:xs) | p x == True = (f x) : (filtraAplica2 f p xs) | otherwise = filtraAplica2 f p xs

--d. foldr
--filtraAplica3 f p xs = foldr (\a acc -> if p a == True then a : acc) [] xs

--16.
horner :: [Double] -> Double -> Double
horner [] z = 0
horner (c:cs) z = c + z *  horner cs z

hornerF :: [Double] -> Double -> Double
hornerF cs z = foldr (\acc a -> a * z + acc) 0 cs

--20.
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) | a > x = x:(insert a xs) | otherwise = a:(x:xs)