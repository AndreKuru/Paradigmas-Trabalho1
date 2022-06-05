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
boxWidth n | n == 4 = 2
           | n == 6 = 2
           | n == 9 = 3
           | otherwise = 0


boxHeight :: Int -> Int
boxHeight n | n == 4 = 2
            | n == 6 = 3
            | n == 9 = 3
            | otherwise = 0

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
    let concatBoxes = concatBoxSegments orderedBoxSegments boxheight
    let boxesPerColumn = div matrixOrder boxheight
    mapColumns boxesPerColumn matrixOrder concatBoxes

-- converte index da matriz de caixas como colunas para index da matrix original
getCorrectIndex :: [Int] -> Int -> [Int]
getCorrectIndex [] _ = []
getCorrectIndex _ 0 = []
getCorrectIndex (a:b) matrixOrder = do
    let indexMatrix = fillNewMatrix matrixOrder matrixOrder False
    nColumns <- b
    let markedIndexMatrix = markMatrix a nColumns indexMatrix
    let boxesIndexMatrix = boxesAsRows(boxesAsRows markedIndexMatrix)
    getElementIndexMatrix True boxesIndexMatrix

markMatrix :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markMatrix row column = setMatrixElement row column True

-- Caixa já estando como linha
-- Checa se a caixa só tem 1 True
checkBox :: [Bool] -> Bool
checkBox = containsOneElement True

-- Caixa já estando como linha
-- Retorna index da primeira caixa encontrada que só tem 1 True
getBoxIndex :: [[Bool]] -> Int
getBoxIndex [] = 0
getBoxIndex (a:b) =
    if checkBox a then
        0
    else
        1 + getBoxIndex b

-- checa todas as caixas por 1 True e retorna index da caixa e da marcação
-- operações extremamente redundantes
getCorrectMarkingIndex :: [[Bool]] -> [Int]
getCorrectMarkingIndex [] = [-1,-1]
getCorrectMarkingIndex boxes = do
    let boxIndex = getBoxIndex boxes
    if boxIndex > 0 && boxIndex < getNRowsMatrix boxes then
        [boxIndex, getElementIndex True (boxes!!boxIndex)]
    else
        [-1, -1]

-- TODO: funçao de checar todas as caixas por 1 único true a partir de uma matriz de marcações normal
-- e retornar o index da marcação pra pôr o número na matriz de números
-- (chamar boxesAsRows dentro da função)
checkAllBoxes :: MarkingsMatrix -> [Int]
checkAllBoxes matrix = do
    let boxRows = boxesAsRows matrix
    getCorrectMarkingIndex boxRows

-- quando é oficialmente anotado o menor número de alguma caixa, é preciso
-- limpar as marcações da linha e coluna do elemento
clearRowAndColumn :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
clearRowAndColumn _ _ [] = []
clearRowAndColumn row column matrix = do
    let clearArray = fillNewArray (getNColumnsMatrix matrix) False
    let partiallyClearMatrix = setMatrixRow row clearArray matrix
    setMatrixColumn column clearArray partiallyClearMatrix