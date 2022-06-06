-- Módulo responsável pelo algoritmo de resolução
module Solver where

import Matrix

-- Checa o comparador a direita do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador da direita do número
checkright :: Int -> Int -> [[Char]] -> Char
checkright row column matrix = (getMatrixOperatorElement (row * 2) column matrix)

-- Checa o comparador a esquerda do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador da esquerda do número
checkleft :: Int -> Int -> [[Char]] -> Char
checkleft _ 0 _ = '|'
checkleft row column matrix = (getMatrixOperatorElement (row * 2) (column - 1) matrix)

-- Checa o comparador debaixo do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador debaixo do número
checkdown :: Int -> Int -> [[Char]] -> Char
checkdown row column matrix = 
    if row == (getNRowsMatrix matrix) then
      '|'
    else
      (getMatrixOperatorElement (row * 2 + 1) (column) matrix)

-- Checa o comparador decima do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador decima do número
checkup :: Int -> Int -> [[Char]] -> Char
checkup row column matrix = 
    if row == 0 then
      '|'
    else
      (getMatrixOperatorElement (row * 2 - 1) (column) matrix)

-- Valida que o número passado não está no array:
-- número -> array -> resposta
validatearraywithx :: Int -> [Int] -> Bool
validatearraywithx _ [] = True
validatearraywithx x (a:b) =  (x /= a) && (validatearraywithx x b)

-- Obs: 0 significa que o elemento não foi inserido ainda e não é contabilizado
-- Valida linha/coluna passada do sudoku:
-- linha -> resposta
validatearray :: [Int] -> Bool
validatearray [] = True
validatearray (a:b) = ((a == 0) || (validatearraywithx a b)) && (validatearray b)

-- Checa para ver se a linha tem elementos repetidos: 
-- linha -> matriz resposta -> matriz de comparadores -> 
validateline :: Int -> [[Int]] -> [[Char]] -> Bool    
validateline row matrixNumber matrixOperator = (validatearray (getMatrixRow row matrixNumber))

-- Checa para ver se a coluna tem elementos repetidos: 
-- coluna -> matriz resposta -> matriz de comparadores -> 
validatecolumn :: Int -> [[Int]] -> [[Char]] -> Bool    
validatecolumn column matrixNumber matrixOperator = (validatearray (getMatrixColumn column matrixNumber))

-- Valida a coerência entre dois elementos, sempre considerando da esquerda pra direita ou de cima para baixo:
-- elemento 1 -> operador -> elemento 2
validateoperation :: Int -> Char -> Int -> Bool
validateoperation 0  _  _ = True
validateoperation _  _  0 = True
validateoperation _ '|' _ = True
validateoperation x '<' y = x < y
validateoperation x '>' y = x > y
validateoperation x 'v' y = x > y
validateoperation x '^' y = x < y

-- Valida a coerência entre o elemento passado e a sua direita
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateright :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validateright row column matrixNumber matrixOperator = 
  (column == (getNColumnsMatrix matrixNumber)) ||
  (validateoperation 
    (getMatrixElement row column matrixNumber) 
    (checkright row column matrixOperator)
    (getMatrixElement row (column + 1) matrixNumber))

-- Valida a coerência entre o elemento passado e a sua esquerda
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateleft :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validateleft row column matrixNumber matrixOperator = 
  (column == 0) ||
  (validateoperation 
    (getMatrixElement row (column - 1) matrixNumber)
    (checkleft row column matrixOperator)
    (getMatrixElement row column matrixNumber))

-- Valida a coerência entre o elemento passado e embaixo
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validatedown :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validatedown row column matrixNumber matrixOperator = 
  (row == (getNRowsMatrix matrixNumber)) ||
  (validateoperation 
    (getMatrixElement row column matrixNumber)
    (checkdown row column matrixOperator)
    (getMatrixElement (row + 1) column matrixNumber))

-- Valida a coerência entre o elemento passado e acima
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateup :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validateup row column matrixNumber matrixOperator = 
  (row == 0) ||
  (validateoperation 
    (getMatrixElement (row - 1) column matrixNumber)
    (checkup row column matrixOperator)
    (getMatrixElement row column matrixNumber))

-- Valida a coerência entre o elemento passado e todos adjacentes
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateadjacents :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validateadjacents row column matrixNumber matrixOperator = 
  (validateright  row column matrixNumber matrixOperator) &&
  (validateleft   row column matrixNumber matrixOperator) &&
  (validatedown   row column matrixNumber matrixOperator) &&
  (validateup     row column matrixNumber matrixOperator)

-- Valida a coerência entre o elemento no sudoku
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validatenumber :: Int -> Int -> [[Int]] -> [[Char]] -> Bool
validatenumber row column matrixNumber matrixOperator = 
  (validateline       row        matrixNumber matrixOperator) &&
  (validatecolumn     column     matrixNumber matrixOperator) &&
  (validateadjacents  row column matrixNumber matrixOperator)

-- TODO checagem de números repetidos numa região ou "caixa"

-- Descobre o valor máximo ou a ordem da matriz de números a partir do tamanho da tabela de operadores:
-- Matriz de operadores -> ordem da matriz de números
getMaxValue :: [[Char]] -> Int
getMaxValue matrixOperator = (div (getNRowsMatrix matrixOperator) 2) + 1

-- Verifica se a matriz sofreu uma exceção do tipo 1, ou seja, em solveElement
validateTry1 :: [[Int]] -> Bool
validateTry1 matrix = (getMatrixElement 3 3 matrix) /= (-1)

-- Verifica se a matriz sofreu uma exceção do tipo 2, ou seja, em solveLine
validateTry2 :: [[Int]] -> Bool
validateTry2 matrix = (getMatrixElement 3 2 matrix) /= (-2)

-- Verifica se a matriz sofreu uma exceção do tipo 3, ou seja, em solveLines
validateTry3 :: [[Int]] -> Bool
validateTry3 matrix = (getMatrixElement 3 1 matrix) /= (-3)

-- Tenta todos os valores num determinado índice com valores a partir do informado:
-- Linha do índice -> Coluna do índice -> valor inical a ser tentado -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveElement :: Int -> Int -> Int -> [[Int]] -> [[Char]] -> [[Int]]
solveElement row column value matrixNumber matrixOperator =  
  if (value > (getMaxValue matrixOperator)) then                              -- Verifica se é já estourou o valor máximo pro chute
    (setMatrixElement 3 3 (-1) try)                                           -- Se sim retorna exceção -1
  else if (validatenumber row column try matrixOperator) then                 -- Verifica se o chute é válido
    try                                                                       -- Se sim devolve o chute
  else
    (solveElement row column (value + 1) matrixNumber matrixOperator)         -- Se não tenta um valor mais alto
  where try = (setMatrixElement row column value matrixNumber)                -- Dá um chute de valor no elemento

-- Tenta todos os elementos numa determinada linha a partir da coluna específicada:
-- Linha selecionada -> Coluna inicial a ser chutada -> value -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveLine :: Int -> Int -> Int -> [[Int]] -> [[Char]] -> [[Int]]
solveLine row column value matrixNumber matrixOperator = 
  if (column >= (getMaxValue matrixOperator)) then  -- Verifica se já chegou ao final da linha
    matrixNumber                                    -- Se sim retorna a linha como está
  else if (validateTry1 try) then                   -- Verifica se tem um chute válido
    if (validateTry2 nextTry) then                  -- Se sim verifica se o chute do próximo elemento é válido
      nextTry                                       --        Se sim retorna o chute dos próximos
    else                                            --
      tryAgain                                      --        Se não tenta um novo chute no elemento atual a partir do último elemento do mesmo
  else                                              --
    (setMatrixElement 3 2 (-2) try)  --test         -- Se não tem um chute válido retorna a exceção -2

--   if (value /= 1) then                 -- test begin
--     (setMatrixElement 3 0 2 (setMatrixElement 3 3 (-5) try))
--   else                                 -- test end
--   if (column  >= (getMaxValue matrixOperator)) then                           -- Verifica se existe mais elementos na linha para chutar
--     matrixNumber
--   else if ((getMatrixElement 3 3 try) == (-1)) then -- test
--     (setMatrixElement 3 2 (-2) try)  --test                                   -- Se não retorna a exceção -2
--   else if ((getMatrixElement 3 2 try) == (-2)) then -- test
--     (solveLine row column lastValue matrixNumber matrixOperator)
--   else                                                                        -- Se não deu exceção é um chute válido, então tenta o próximo valor
--     if ((getMatrixElement 3 3 nextTry) == (-2)) then -- test
--       (solveLine row column lastValue matrixNumber matrixOperator)
--       
-- 
  where try =       (solveElement row column value matrixNumber matrixOperator)           -- Dá um chute num elemento da linha
        tryAgain  = (solveElement row column (lastValue + 1) try matrixOperator) -- Dá um novo chute num elemento da linha
        lastValue = (getMatrixElement row column try)                            -- Valor do último chute
        nextTry   = (solveLine row (column + 1) 1 try matrixOperator)                    -- Chuta o próximo elemento

-- Tenta todas as linhas de uma determinada matriz a partir da linha informada:
-- Linha inicial -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveLines :: Int -> [[Int]] -> [[Char]] -> [[Int]]
solveLines row matrixNumber matrixOperator =  try
--  if
--  (row >= (getMaxValue matrixOperator)) then                                  -- Verifica se existe mais linhas para chutar
--    matrixNumber
--
--  else if
--  ((getMatrixElement 3 2 try) /= (-2)) -- test
--  -- (try /= [[-2]]) &&                                                       -- Verifica se o chute deu certo
--  then
--                                                                               
--    (solveLines (row + 1) try matrixOperator)                                 -- Se sim chuta chuta a próxima linha a partir das linhas anteriores
--                                                                               
--  else                                                                         
--    -- [[-3]]                                                                 -- Se não retorna exceção
--    (setMatrixElement 3 1 (-3) try)  --test
--                                                                               
  where try = (solveLine row 0 1 matrixNumber matrixOperator)                 --  Tenta solucionar a linha informada desde o primeiro elemento

-- Cria uma matriz zerada de ordem N:
-- N --> Matriz de números zerada
createMatrixNumber :: Int -> [[Int]]
createMatrixNumber n = fillNewMatrix n n 0

-- Encontra uma matriz de números solução para uma matriz de operadores passada:
-- Matriz de operadores -> Matriz de números
solveMatrix :: [[Char]] -> [[Int]]
solveMatrix matrixOperator = 
  (solveLines 
    0
    matrixNumber
    matrixOperator
  )
    where matrixNumber = createMatrixNumber (getMaxValue matrixOperator)