-- Módulo responsável pelo algoritmo de resolução
module Solver where

import Matrix, Operators

-- Checa o comparador a direita do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador da direita do número
checkright :: Int -> Int -> [[t]] -> t
checkright row column matrix = (getMatrixOperatorElement (row * 2) column matrix)

-- Checa o comparador a esquerda do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador da esquerda do número
checkleft :: Int -> Int -> [[t]] -> t
checkleft _ 0 _ = "|"
checkleft row column matrix = (getMatrixOperatorElement (row * 2) (column - 1) matrix)

-- Checa o comparador debaixo do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador debaixo do número
checkdown :: Int -> Int -> [[t]] -> t
checkdown row column matrix = 
    if row == (getNRowsMatrix matrix) then
      "|"
    else
      (getMatrixOperatorElement (row * 2 + 1) (column) matrix)

-- Checa o comparador decima do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador decima do número
checkup :: Int -> Int -> [[t]] -> t
checkup row column matrix = 
    if row == 0 then
      "|"
    else
      (getMatrixOperatorElement (row * 2 - 1) (column) matrix)