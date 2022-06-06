module Operators where

-- A matriz é uma lista de linhas
type Array = [t]
type Matrix = [Array]

createEmptyArray :: Int -> Array
createEmptyArray 0 = []
createEmptyArray n = [""] ++ createEmptyArray (n-1)

createEmptyMatrix :: Int -> Int -> Matrix
createEmptyMatrix 0 _ = []
createEmptyMatrix _ 0 = []
createEmptyMatrix 1 n = [createEmptyArray n]
createEmptyMatrix n 1 = [''] ++ createEmptyMatrix (n-1) 1
createEmptyMatrix n m = [createEmptyArray m] ++ createEmptyMatrix (n-1) m

getArrayLength :: Array -> Int
getArrayLength [] = 0
getArrayLength (a:b) = 1 + getArrayLength b

splitArray :: Int -> Int -> Array -> Array
splitArray _ _ [] = []
splitArray 0 0 _ = []
splitArray 0 end (a:b) = [a] ++ splitArray 0 (end-1) b
splitArray beginning 0 (a:b) = [a]
splitArray beginning end (a:b) =
    [] ++ splitArray (beginning-1) (end-1) b

setArrayElement :: Int -> t -> Array -> Array
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
getMatrixRow n (a:b) = getMatrixRow (n-1) b

getNColumnsMatrix :: Matrix -> Int
getNColumnsMatrix [] = 0
getNColumnsMatrix (a:b) = getArrayLength a

getMatrixColumn :: Int -> Matrix -> Array
getMatrixColumn _ [] = []
getMatrixColumn n (a:b) =
    if n >= 0 && (n + 1) < getNColumnsMatrix (a:b) then
        [a!!n] ++ getMatrixColumn n b
    else
        []

getGenericMatrixElement :: Int -> Int -> Matrix -> t
getGenericMatrixElement _ _ [] = " "
getGenericMatrixElement row column matrix =
    if column >= 0 && (column + 1) < getNColumnsMatrix matrix then
        getMatrixRow row matrix!!column
    else
        " "

splitMatrixLines :: Int -> Int -> Matrix -> Matrix
splitMatrixLines _ _ [] = []
splitMatrixLines 0 0 _ = []
splitMatrixLines 0 end (a:b) = [a] ++ splitMatrixLines 0 (end-1) b
splitMatrixLines beginning 0 (a:b) = [a]
splitMatrixLines beginning end (a:b) =
    [] ++ splitMatrixLines (beginning-1) (end-1) b

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

setMatrixElement :: Int -> Int -> t -> Matrix -> Matrix
setMatrixElement _ _ _ [] = []
setMatrixElement row column value matrix = do
    let line = getMatrixRow row matrix
    let newRow = setArrayElement column value line
    setMatrixRow row newRow matrix

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
