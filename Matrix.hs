{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}

module Matrix where

type Array = [Int]
type Matrix = [Array]
-- a matriz é uma lista de linhas

createEmptyArray :: Int -> Array
createEmptyArray 0 = []
createEmptyArray n = [0] ++ createEmptyArray j
    where j = n-1

createEmptyMatrix :: Int -> Int -> Matrix
createEmptyMatrix 0 _ = []
createEmptyMatrix _ 0 = []
createEmptyMatrix 1 n = [createEmptyArray n]
createEmptyMatrix n 1 = [[0]] ++ createEmptyMatrix j 1
    where j = n-1
createEmptyMatrix n m = [createEmptyArray m] ++ createEmptyMatrix j m
    where j = n-1

getArrayLength :: Array -> Int
getArrayLength [] = 0
getArrayLength (a:b) = 1 + getArrayLength b

-- todo: ainda tá errado (se beggining > end ou se inserir número negativo dá errado)
-- por enquanto, o começo é inclusivo mas o final não
-- ex: splitArray 2 4 [1,2,3,4,5] = [3,4]
-- se end for maior que o comprimento do array, só ignora o que sobra
splitArray :: Int -> Int -> Array -> Array
splitArray _ _ [] = []
splitArray 0 0 _ = []
splitArray 0 end (a:b) = [a] ++ splitArray 0 (end-1) b
splitArray beginning 0 (a:b) = [a]
splitArray beginning end (a:b) =
        []++splitArray (beginning-1) (end-1) b

setArrayElement :: Int -> Int -> Array -> Array
setArrayElement _ _ [] = []
setArrayElement index value array =
    if index < 0 || index >= arrayLength then
        array
    else
        splitArray 0 index array ++ [value] ++ splitArray (index+1) arrayLength array
    where arrayLength = getArrayLength array

getNRowsMatrix :: Matrix -> Int
getNRowsMatrix [] = 0
getNRowsMatrix (a:b) = 1 + getNRowsMatrix b

getMatrixRow :: Int -> Matrix -> Array
getMatrixRow _ [] = []
getMatrixRow 0 (a:b) = a
getMatrixRow n (a:b) = getMatrixRow m b
    where m = n-1

-- assume que todas as linhas tem o mesmo comprimento
getNColumnsMatrix :: Matrix -> Int
getNColumnsMatrix [] = 0
getNColumnsMatrix (a:b) = getArrayLength a

getMatrixColumn :: Int -> Matrix -> Array
getMatrixColumn _ [] = []
getMatrixColumn n (a:b) =
    if n >= 0 && n + 1 < getNColumnsMatrix (a:b) then
        [a!!n] ++ getMatrixColumn n b
    else
        []

-- no tabuleiro, todos os números válidos são positivos
getMatrixElement :: Int -> Int -> Matrix -> Int
getMatrixElement _ _ [] = - 1
getMatrixElement row column matrix =
    if column >= 0 && column + 1 < getNColumnsMatrix matrix then
        getMatrixRow row matrix!!column
    else
        (-1)

-- todo: ainda tá errado (se beggining > end ou se inserir número negativo dá errado)
-- por enquanto, o começo é inclusivo mas o final não
-- ex: splitArray 2 4 [1,2,3,4,5] = [3,4]
-- se end for maior que o número de linhas da matriz, só ignora o que sobra
splitMatrixLines :: Int -> Int -> Matrix -> Matrix
splitMatrixLines _ _ [] = []
splitMatrixLines 0 0 _ = []
splitMatrixLines 0 end (a:b) = [a] ++ splitMatrixLines 0 (end-1) b
splitMatrixLines beginning 0 (a:b) = [a]
splitMatrixLines beginning end (a:b) =
        []++splitMatrixLines (beginning-1) (end-1) b

setMatrixRow :: Int -> Array -> Matrix -> Matrix
setMatrixRow _ _ [] = []
setMatrixRow rowNumber row matrix =
    if rowNumber >= matrixLength || rowNumber < 0 then
        matrix
    else
        splitMatrixLines 0 rowNumber matrix ++ [row] ++ splitMatrixLines (rowNumber+1) matrixLength matrix
    where matrixLength = getNRowsMatrix matrix

setMatrixColumn :: Int -> Array -> Matrix -> Matrix
setMatrixColumn _ _ [] = []
setMatrixColumn _ [] matrix = matrix
setMatrixColumn column (a:b) (c:d) = 
    [setArrayElement column a c] ++ setMatrixColumn column b d

setMatrixElement :: Int -> Int -> Int -> Matrix -> Matrix
setMatrixElement _ _ _ [] = []
setMatrixElement row column value matrix = do
    let line = getMatrixRow row matrix
    let newRow = setArrayElement column value line
    setMatrixRow row newRow matrix

-- Talvez devesse ser feito em uma classe, não sei
printMatrix :: Matrix -> IO ()
printMatrix [] = putStrLn []
printMatrix (a:b) = do
    putStrLn(arrayString a)
    printMatrix b

matrixString :: Matrix -> String
matrixString [] = []
matrixString (a:b) = arrayString a ++ matrixString b

arrayString :: Array -> String
arrayString [] = []
arrayString (a:b) = show a ++ " " ++ arrayString b