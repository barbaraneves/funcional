-- Capitulo 3: Uma Visão Funcional da Indução --

{- Questão 2 -}
multiplos_n3 n1 n2 n3 | (n2 <= n1) || (n2 - n1 == 1) = 0
                      | (mod (n2 - 1) n3) == 0 = 1 + (multi_int n1 (n2 - 1) n3)
                      | (mod (n2 - 1) n3) /= 0 = 0 + (multi_int n1 (n2 - 1) n3)



