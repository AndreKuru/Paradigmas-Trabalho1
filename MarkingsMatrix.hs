{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}

module MarkingsMatrix where

import Matrix

type MarkingsArray = [Bool]
type MarkingsMatrix = [MarkingsArray]

-- insere ordem da matriz para descobrir tamanho das "caixas" de comparadores
boxWidth :: Int -> Int
boxWidth 4 = 2
boxWidth 6 = 2
boxWidth 9 = 3

boxHeight :: Int -> Int
boxHeight 4 = 2
boxHeight 6 = 3
boxHeight 9 = 3

-- a partir daqui até antes de boxesAsRows é tudo função auxiliar
splitRowPerBoxes :: Int -> Int -> [t] -> [[t]]
splitRowPerBoxes _ 0 _ = []
splitRowPerBoxes 0 _ row = []
splitRowPerBoxes _ _ [] = []
splitRowPerBoxes boxWidth boxesPerRow row =
    --boxesPerRow = getArrayLength (a:b) / boxWidth
    [splitArray 0 boxWidth row]++splitRowPerBoxes boxWidth boxAmount newRow
    where newRow = splitArray boxWidth rowLength row
          boxAmount = boxesPerRow-1
          rowLength = getArrayLength row

splitMatrixPerBoxesPerLine :: Int -> Int -> [[t]] -> [[t]]
splitMatrixPerBoxesPerLine _ 0 _ = []
splitMatrixPerBoxesPerLine 0 _ _ = []
splitMatrixPerBoxesPerLine _ _ [] = []
splitMatrixPerBoxesPerLine boxWidth boxesPerRow (a:b) =
    splitRowPerBoxes boxWidth boxesPerRow a ++ splitMatrixPerBoxesPerLine boxWidth boxesPerRow b

mapColumns :: Int -> Int -> [[t]] -> [[t]]
mapColumns 0 _ _ = []
mapColumns _ 0 _ = []
mapColumns _ _ [] = []
mapColumns boxesPerRow matrixOrder matrixSplitPerBoxesPerLine =
    mapColumnsAllIterations boxesPerRow boxesPerRow matrixOrder matrixSplitPerBoxesPerLine

mapColumnsIteration :: Int -> Int -> Int -> [[t]] -> [[t]]
mapColumnsIteration 0 _ _ _ = []
mapColumnsIteration _ _ _ [] = []
mapColumnsIteration _ _ 0 _ = []
mapColumnsIteration boxesPerRow i matrixOrder (a:b) | i > (matrixOrder * boxesPerRow) = []
                                        | i `mod` boxesPerRow == 0 = [a]++ mapColumnsIteration boxesPerRow (i+1) matrixOrder b
                                        | otherwise = []++ mapColumnsIteration boxesPerRow (i+1) matrixOrder b
                                        where j = i+1

-- mapColumnsAllIterations :: boxesPerRow -> numOfIterations ->  matrixOrder -> matrix -> matrix
mapColumnsAllIterations :: Int -> Int -> Int -> [[t]] -> [[t]]
mapColumnsAllIterations 0 _ _ _ = []
mapColumnsAllIterations _ _ _ [] = []
mapColumnsAllIterations _ 0 _ _ = []
mapColumnsAllIterations _ _ 0 _ = []
mapColumnsAllIterations boxesPerRow numOfIterations matrixOrder (a:b) =
    mapColumnsIteration boxesPerRow 0 matrixOrder (a:b) ++ mapColumnsAllIterations boxesPerRow (numOfIterations-1) matrixOrder b

concatMatrixLines :: Int -> Int -> [[t]] -> [t]
concatMatrixLines _ _ [] = []
concatMatrixLines 0 0 _ = []
concatMatrixLines 0 end (a:b) = a ++ concatMatrixLines 0 (end-1) b
concatMatrixLines beginning 0 (a:b) = a
concatMatrixLines beginning end (a:b) =
        []++concatMatrixLines (beginning-1) (end-1) b

concatBoxSegments :: [[t]] -> Int -> [[t]]
concatBoxSegments [] _ = []
concatBoxSegments _ 0 = []
concatBoxSegments segments boxHeight =
    [concatMatrixLines 0 boxHeight segments]++concatBoxSegments newRow boxHeight
    where newRow = splitMatrixLines boxHeight numberOfSegments segments
          numberOfSegments = getNRowsMatrix segments

-- faz com que cada linha seja equivalente a uma caixa
-- assume que a matriz é quadrada
-- se eu pôr que recebe MarkingsMatrix não compila e não sei arrumar agora :v
boxesAsRows :: [[t]] -> [[t]]
boxesAsRows matrix = do
    let matrixOrder = getNColumnsMatrix matrix
    let boxwidth = boxWidth matrixOrder
    let boxheight = boxHeight matrixOrder
    let boxesPerRow = div matrixOrder boxwidth
    let boxSegments = splitMatrixPerBoxesPerLine boxwidth boxesPerRow matrix
    let orderedBoxSegments = mapColumns boxesPerRow matrixOrder boxSegments
    concatBoxSegments orderedBoxSegments boxheight

markMatrix :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markMatrix row column = setMatrixElement row column True

-- TODO: funçao de checar todas as caixas por 1 único true a partir de uma matriz de marcações normal
-- (chamar boxesAsRows dentro da função)

-- TODO: função de contains em um array

-- quando é oficialmente anotado o menor número de alguma caixa, é preciso 
-- limpar as marcações da linha e coluna do elemento
clearRowAndColumn :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
clearRowAndColumn _ _ [] = []
clearRowAndColumn row column matrix = do
    let clearArray = fillNewArray (getNColumnsMatrix matrix) False
    let partiallyClearMatrix = setMatrixRow row clearArray matrix
    setMatrixColumn column clearArray partiallyClearMatrix

printMarkingsMatrix :: MarkingsMatrix -> IO ()
printMarkingsMatrix [] = putStrLn []
printMarkingsMatrix (a:b) = do
    putStrLn(markingsArrayString a)
    printMarkingsMatrix b

markingsMatrixString :: MarkingsMatrix -> String
markingsMatrixString [] = []
markingsMatrixString (a:b) = markingsArrayString a ++ markingsMatrixString b

markingsArrayString :: MarkingsArray -> String
markingsArrayString [] = []
markingsArrayString (a:b) = show a ++ " " ++ markingsArrayString b