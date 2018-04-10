-- Questão valendo 0,5: Dada uma lista, defina uma 
-- função comp que retorne o comprimento da lista --

comp [] = 0
comp (cabeca : x) = 1 + comp x -- x representa os elementos da lista.
