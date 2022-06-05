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
checkleft _ 0 _ = '|'
checkleft row column matrix = (getMatrixOperatorElement (row * 2) (column - 1) matrix)

-- Checa o comparador debaixo do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador debaixo do número
checkdown :: Int -> Int -> [[t]] -> t
checkdown row column matrix = 
    if row == (getNRowsMatrix matrix) then
      '|'
    else
      (getMatrixOperatorElement (row * 2 + 1) (column) matrix)

-- Checa o comparador decima do elemento numa matriz de 4: 
-- linha do número -> coluna do número -> matriz de comparadores -> comparador decima do número
checkup :: Int -> Int -> [[t]] -> t
checkup row column matrix = 
    if row == 0 then
      '|'
    else
      (getMatrixOperatorElement (row * 2 - 1) (column) matrix)

-- Valida que o número passado não está no array:
-- número -> array -> resposta
validatearraywithx :: Int -> [[Int]] -> Bool
validatearraywithx _ [] = True
validatearraywithx x (a:b) =  (x != a) && (validatearraywithx x b)

-- Obs: 0 significa que o elemento não foi inserido ainda e não é contabilizado
-- Valida linha/coluna passada do sudoku:
-- linha -> resposta
validatearray :: [Int] -> Bool
validatearray [] = True
validatearray (a:b) = ((a == 0) || (validatearraywithx a b)) && (validatearray b)

-- Checa para ver se a linha tem elementos repetidos: 
-- linha -> matriz resposta -> matriz de comparadores -> 
validateline :: Int -> [[Int]] -> [[t]] -> Bool    
validateline row matrixNumber matrixOperator = (validatearray (getMatrixRow row matrixNumber))

-- Checa para ver se a coluna tem elementos repetidos: 
-- coluna -> matriz resposta -> matriz de comparadores -> 
validatecolumn :: Int -> [[Int]] -> [[t]] -> Bool    
validateline column matrixNumber matrixOperator = (validatedown (getMatrixColumn column matrixNumber))

-- Valida a coerência entre dois elementos, sempre considerando da esquerda pra direita ou de cima para baixo:
-- elemento 1 -> operador -> elemento 2
validateoperation :: Int -> Char -> Int -> Bool
validateoperation _ '|' _ = True
validateoperation x '<' y = x < y
validateoperation x '>' y = x > y
validateoperation x 'v' y = x > y
validateoperation x '^' y = x < y

-- Valida a coerência entre o elemento passado e a sua direita
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateright :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validateright row column matrixNumber matrixOperator = 
  (column == (getNColumnsMatrix matrixNumber)) ||
  (validateoperation 
    (getMatrixElement row column matrixNumber) 
    (checkright row column matrixOperator)
    (getMatrixElement (row + 1) column matrixNumber))

-- Valida a coerência entre o elemento passado e a sua esquerda
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateleft :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validateleft row column matrixNumber matrixOperator = 
  (column == 0) ||
  (validateoperation 
    (getMatrixElement (row - 1) column matrixNumber)
    (checkright row column matrixOperator)
    (getMatrixElement row column matrixNumber))

-- Valida a coerência entre o elemento passado e embaixo
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validatedown :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validatedown row column matrixNumber matrixOperator = 
  (row == (getNRowsMatrix matrixNumber)) ||
  (validateoperation 
    (getMatrixElement row column matrixNumber)
    (checkright row column matrixOperator)
    (getMatrixElement row (column + 1) matrixNumber))

-- Valida a coerência entre o elemento passado e acima
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateup :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validateup row column matrixNumber matrixOperator = 
  (column == 0) ||
  (validateoperation 
    (getMatrixElement row (column - 1) matrixNumber)
    (checkright row column matrixOperator)
    (getMatrixElement row column matrixNumber))

-- Valida a coerência entre o elemento passado e todos adjacentes
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validateadjacents :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validateadjacents row column matrixNumber matrixOperator = 
  (validateright  row column matrixNumber matrixOperator) &&
  (validateleft   row column matrixNumber matrixOperator) &&
  (validatedown   row column matrixNumber matrixOperator) &&
  (validateup     row column matrixNumber matrixOperator)

-- Valida a coerência entre o elemento no sudoku
-- linha -> coluna -> matriz de números -> matriz de operadores -> mantem coerência?
validatenumber :: Int -> Int -> [[Int]] -> [[t]] -> Bool
validatenumber row column matrixNumber matrixOperator = 
  (validateline       row        matrixNumber matrixOperator) &&
  (validatecolumn     column     matrixNumber matrixOperator) &&
  (validateadjacents  row column matrixNumber matrixOperator)

-- TODO checagem de números repetidos numa região ou "caixa"

-- Descobre o valor máximo ou a ordem da matriz de números a partir do tamanho da tabela de operadores:
-- Matriz de operadores -> ordem da matriz de números
getMaxValue :: [[t]] -> Int
getMaxValue matrixOperator = div (getNRowsMatrix matrixOperator) 2

-- Tenta todos os valores num determinado índice com valores a partir do informado:
-- Linha do índice -> Coluna do índice -> valor inical a ser tentado -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveElement :: Int -> Int -> Int -> [[Int]] -> [[t]] -> [[Int]]
solveElement row column value matrixNumber matrixOperator =  
  if (validatenumber row column try matrixOperator) then                      -- Verifica se o chute deu certo
    try                                                                       -- Se sim devolve o chute
  else if (value + 1 <= (getMaxValue matrixOperator)) then                    -- Se não vê se pode tentar outro chute
    (solveElement row column (value + 1) matrixNumber matrixOperator)         -- Se pode então tenta
  else                                                                        -- Se não pode devolve a exceção
    [[-1]]
  where try = (setMatrixElement row column value matrixNumber matrixOperator) -- Dá um chute de valor no elemento

-- Tenta todos os elementos numa determinada linha a partir da coluna específicada:
-- Linha selecionada -> Coluna inicial a ser chutada -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveLine :: Int -> Int -> [[Int]] -> [[t]] -> [[Int]]
solveLine row column matrixNumber matrixOperator =  
  if 
  (try != [[-1]]) &&                                                          -- Verifica se o chute deu certo
  ((column + 1) < (getMaxValue matrixOperator)) then                          -- Verifica se existe mais elementos na linha para chutar
    
    (solveLine row (column + 1) try matrixOperator)                           -- Se sim chuta o próximo elemento a partir do último chute
    
  else                                                                        -- Se não pode devolve a exceção
    try
  where try = (solveElement row column 1 matrixNumber matrixOperator)         -- Dá um chute num elemento da linha

-- Tenta todas as linhas de uma determinada matriz a partir da linha informada:
-- Linha inicial -> Matriz de números -> Matriz de operadores -> Matriz resposta
solveLines :: Int -> [[Int]] -> [[t]] -> [[Int]]
solveLines row matrixNumber matrixOperator =  
  if
  (try != [[-1]]) &&                                                           -- Verifica se o chute deu certo
  ((line + 1) < (getMaxValue matrixOperator)) then                             -- Verifica se existe mais linhas para chutar
                                                                               
    (solveLines (row + 1) try matrixOperator)                                  -- Se sim chuta chuta a próxima linha a partir das linhas anteriores
                                                                               
  else                                                                         -- 
    try                                                                        -- Se não retorna exceção
                                                                               
  where try = (solveLine row 0 matrixNumber matrixOperator)                    --  Tenta solucionar a linha informada desde o primeiro elemento

-- Cria uma matriz zerada de ordem N:
-- N --> Matriz de números zerada
CreateMatrixNumber :: Int -> [[Int]]
CreateMatrixNumber n = fillNewMatrix n n 0

-- Encontra uma matriz de números solução para uma matriz de operadores passada:
-- Matriz de operadores -> Matriz de números
solveMatrix :: [[t]] -> [[Int]]
solveMatrix matrixOperator =
  (solveLines 
    0
    matrixNumber
    matrixOperator
  )
    where matrixNumber = CreateMatrixNumber (getMaxValue matrixOperator)