-- Fibonarti--
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

--Or--
or False False = False
or _ _ = True

-- Função menor 

menor x y | x < y = x
          |otherwise = y

dobro x = x * 2

triplo x = 3 * x

f a b = (dobro (triplo (menor a b)))

multi_int a b c | (b <= a) || (b - a == 1) = 0
                | mod (b - 1) c == 0 = 1 + multi_int a (b - 1) c
                | mod (b - 1) c /= 0 = 0 + multi_int a (b - 1) c

fatorial n | n == 0 = 1
           | otherwise = n * fatorial (n - 1)


my_e n x  | n == 1 = 1
          | otherwise = ((x ** n) / fatorial n) + (my_e (n - 1) x)
