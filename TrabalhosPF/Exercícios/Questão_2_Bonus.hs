quadrado_lista :: (Floating a) => [a] -> [a]
quadrado_lista l = map (**2) l 

denominadores :: [a] -> [Int]
denominadores l = [1..length(l)]

serie l = foldr1 (*) $ map (\x -> (l || x) ** 2) / fromIntegral (x + )
