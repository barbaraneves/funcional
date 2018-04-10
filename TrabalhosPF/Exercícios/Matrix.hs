type Row = [Float]

data Matrix = Matrix 
{ ncols::Int
, nrows::Int
, rows::[Row]
}
Instace Show Matrix


module Matriz
{ zeroMatriz
, oneMatriz
, identMatriz
, sumMatriz
, prodScalar
, prodMatriz
, listToMatriz 
} where

	zeroMatriz::Int -> Int -> Matrix

	oneMatriz::Int -> Int -> Matrix

	identMatriz::Int -> Matrix

	sumMatriz::Matrix -> Matrix -> Matrix

	prodScalar::Float -> Matrix -> Matrix

	prodMatriz::Matrix -> Matrix -> Matrix

	listToMatriz::[Row] -> Matrix
