tipo_triangulo :: (Float, Float, Float) -> (String, Float)
tipo_triangulo (a, b, c) | a == b && b == c = ("Equilátero", a + b + c)
                         | a == b && b /= c = ("Isósceles", a + b + c)
                         | a /= b && b == c = ("Isósceles", a + b + c)
                         | a == c && b /= c = ("Isósceles", a + b + c)
			 | otherwise = ("Escaleno", a + b + c)

triangulo :: (Float, Float, Float) -> (String, Float)
triangulo (a, b, c) | abs (b - c) < a && a < b + c = tipo_triangulo (a, b, c)
                    | abs (a - c) < b && b < a + c = tipo_triangulo (a, b, c)
		    | abs (a - b) < c && c < a + b = tipo_triangulo (a, b, c)
		    | otherwise = ("Não é Triângulo", 0)