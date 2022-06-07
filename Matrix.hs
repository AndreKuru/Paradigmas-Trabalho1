{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use String" #-}

module Matrix where

type Array = [Int]
type Matrix = [Array]
-- a matriz é uma lista de linhas

-- *Preencher a matriz com o valor passado: 
-- quantidade de linhas -> quantidade de colunas -> valor a preencher -> matriz preenchida
fillNewMatrix :: Int -> Int -> t -> [[t]]
fillNewMatrix 0 _ _ = []
fillNewMatrix _ 0 _ = []
fillNewMatrix 1 numColumns value = [fillNewArray numColumns value]
fillNewMatrix numRows 1 value = [[value]] ++ fillNewMatrix (numRows-1) 1 value
fillNewMatrix numRows numColumns value = [fillNewArray numColumns value] ++ fillNewMatrix (numRows-1) numColumns value

-- *Preenche um array com um determinado valor: 
-- tamanho -> valor para preencher -> array preenchido
fillNewArray :: Int -> t -> [t]
fillNewArray 0 _ = []
fillNewArray size value = [value] ++ (fillNewArray (size - 1) value)

-- *Retorna o tamanho de um array: array -> tamanho
getArrayLength :: [t] -> Int
getArrayLength [] = 0
getArrayLength (a:b) = 1 + getArrayLength b

-- todo: ainda tá errado (se beggining > end ou se inserir número negativo dá errado)
-- por enquanto, o começo é inclusivo mas o final não
-- ex: splitArray 2 4 [1,2,3,4,5] = [3,4]
-- se end for maior que o comprimento do array, só ignora o que sobra

-- *Seleciona um trecho de array: começo -> fim -> array -> trecho selecionado
splitArray :: Int -> Int -> [t] -> [t]
splitArray _ _ [] = []
splitArray 0 0 _ = []
splitArray 0 end (a:b) = [a] ++ splitArray 0 (end-1) b
splitArray beginning 0 (a:b) = [a]
splitArray beginning end (a:b) =
        []++splitArray (beginning-1) (end-1) b

-- *Definir um elemento num array: Posição do elemento -> valor a ser definido ->  array -> array modificado
setArrayElement :: Int -> t -> [t] -> [t]
setArrayElement _ _ [] = []
setArrayElement index value array =
    if index < 0 || index >= arrayLength then
        array
    else
        splitArray 0 index array ++ [value] ++ splitArray (index+1) arrayLength array
    where arrayLength = getArrayLength array

containsElement :: (Eq t) => t -> [t] -> Bool
containsElement _ [] = False
containsElement element (a:b) = (a == element) || containsElement element b

countElementOcurrences :: (Eq t) => t -> [t] -> Int
countElementOcurrences _ [] = 0
countElementOcurrences element (a:b) | a == element = 1 + countElementOcurrences element b
                                     | otherwise = 0 + countElementOcurrences element b

containsOneElement :: (Eq t) => t -> [t] -> Bool
containsOneElement _ [] = False
containsOneElement element array =
    countElementOcurrences element array == 1

getElementIndex :: (Eq t) => t -> [t] -> Int
getElementIndex _ [] = 0
getElementIndex element (a:b) =
    if containsElement element [a] then
        0
    else
        1 + getElementIndex element b

-- *Retorna a quantidade de linhas da matriz: matriz -> quantidade de linhas
getNRowsMatrix :: [[t]] -> Int
getNRowsMatrix [] = 0
getNRowsMatrix (a:b) = 1 + getNRowsMatrix b

-- *Retorna uma linha da matriz: número da linha -> matriz -> linha
getMatrixRow :: Int -> [[t]] -> [t]
getMatrixRow _ [] = []
getMatrixRow 0 (a:b) = a
getMatrixRow rowIndex (a:b) = getMatrixRow (rowIndex-1) b

-- assume que todas as linhas tem o mesmo comprimento
-- *Retorna a quantidade de colunas da matriz: 
-- matriz -> quantidade de colunas
getNColumnsMatrix :: [[t]] -> Int
getNColumnsMatrix [] = 0
getNColumnsMatrix (a:b) = getArrayLength a

-- *Retorna a coluna da matriz: posição da coluna -> matriz -> coluna selecionada
getMatrixColumn :: Int -> [[t]] -> [t]
getMatrixColumn _ [] = []
getMatrixColumn n (a:b) =
    if n >= 0 && n + 1 < getNColumnsMatrix (a:b) then
        [a!!n] ++ getMatrixColumn n b
    else
        []

-- no tabuleiro, todos os números válidos são positivos
-- *Retorna elemento da matriz:
-- linha do elemento -> coluna do elemento -> Matriz -> elemento
getMatrixElement :: Int -> Int -> Matrix -> Int
getMatrixElement _ _ [] = - 1
getMatrixElement row column matrix =
    if (column >= 0) && (column < getNColumnsMatrix matrix)  && (row >= 0) && (row < getNRowsMatrix matrix) then
        (getMatrixRow row matrix)!!column
    else
        (-1)

-- Retorna elemento da matriz de operadores:
-- linha -> coluna -> matriz -> operador
getOperatorMatrixElement :: Int -> Int -> [[Char]] -> Char
getOperatorMatrixElement row column matrix = 
    if (column >= 0) && (column < getNColumnsMatrix matrix) && (row >= 0) && (row < getNRowsMatrix matrix) then
        (getMatrixRow row matrix)!!column
    else
        '|'
        
-- todo: ainda tá errado (se beggining > end ou se inserir número negativo dá errado)
-- por enquanto, o começo é inclusivo mas o final não
-- ex: splitArray 2 4 [1,2,3,4,5] = [3,4]
-- se end for maior que o número de linhas da matriz, só ignora o que sobra
-- Útil para modificar uma matriz
-- Retorna as linhas selecionadas: começo -> final -> matriz -> seleção de linhas
splitMatrixLines :: Int -> Int -> [[t]] -> [[t]]
splitMatrixLines _ _ [] = []
splitMatrixLines 0 0 _ = []
splitMatrixLines 0 end (a:b) = [a] ++ splitMatrixLines 0 (end-1) b
splitMatrixLines beginning 0 (a:b) = [a]
splitMatrixLines beginning end (a:b) =
        []++splitMatrixLines (beginning-1) (end-1) b

-- Modifica uma linha de uma matriz: posição da linha -> nova linha -> matriz a ser modificada -> matriz modificada
setMatrixRow :: Int -> [t] -> [[t]] -> [[t]]
setMatrixRow _ _ [] = []
setMatrixRow rowNumber row matrix =
    if rowNumber >= matrixLength || rowNumber < 0 then
        matrix
    else
        splitMatrixLines 0 rowNumber matrix ++ [row] ++ splitMatrixLines (rowNumber+1) matrixLength matrix
    where matrixLength = getNRowsMatrix matrix

-- *Modifica uma coluna de uma matriz: posição da coluna -> nova coluna -> matriz a ser modificada -> matriz modificada
setMatrixColumn :: Int -> [t] -> [[t]] -> [[t]]
setMatrixColumn _ _ [] = []
setMatrixColumn _ [] matrix = matrix
setMatrixColumn column (a:b) (c:d) = 
    [setArrayElement column a c] ++ setMatrixColumn column b d

-- *Modifica um elemento de uma matriz:
-- linha do elemento -> coluna do elemento -> valor do elemento -> matriz a ser modificada -> matriz modificada
setMatrixElement :: Int -> Int -> t -> [[t]] -> [[t]]
setMatrixElement _ _ _ [] = []
setMatrixElement row column value matrix = do
    let line = getMatrixRow row matrix
    let newRow = setArrayElement column value line
    setMatrixRow row newRow matrix

getRowIndexThatContains :: (Eq t) => t -> [[t]] -> Int
getRowIndexThatContains _ [] = 0
getRowIndexThatContains element (a:b) =
    if containsElement element a then
        0
    else
        1 + getRowIndexThatContains element b

getElementIndexMatrix :: (Eq t) => t -> [[t]] -> (Int, Int)
getElementIndexMatrix _ [] = (-1,-1)
getElementIndexMatrix element matrix = do
    let rowIndex = getRowIndexThatContains element matrix
    if rowIndex >= 0 && rowIndex < getNRowsMatrix matrix then
        (rowIndex, getElementIndex element (matrix!!rowIndex))
    else
        (-1,-1)
    
-- *Imprime a matriz: matriz a ser impressa -> IO
printMatrix :: (Show t) => [[t]] -> IO ()
printMatrix [] = putStrLn []
printMatrix (a:b) = do
    putStrLn(arrayString a)
    printMatrix b

-- Transforma um array em uma linha 
arrayString :: (Show t) => [t] -> String
arrayString [] = []
arrayString (a:b) = show a ++ " " ++ arrayString b
