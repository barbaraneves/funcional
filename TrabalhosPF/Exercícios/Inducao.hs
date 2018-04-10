soma 1 = 1
soma n = n + soma (n - 1)

fatorial_tipo_1 0 = 1
fatorial_tipo_1 n = n * fatorial_tipo_1 (n - 1)

fatorial_tipo_2 n | n == 0 = 1
                  | otherwise = n * fatorial_tipo_2 (n - 1) 
