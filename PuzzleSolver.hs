{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use String" #-}
module PuzzleSolver where

import Matrix
import MarkingsMatrix

type OperatorMatrix = [[Char]]
type Operator = Char

type NumbersMatrix = [[Int]]

getRightOperator :: Int -> Int -> OperatorMatrix -> Operator
getRightOperator numberRowIndex = getOperatorMatrixElement (numberRowIndex * 2)

getLeftOperator :: Int -> Int -> OperatorMatrix -> Operator
getLeftOperator _ 0 _ = '|'
getLeftOperator numberRowIndex numberColumnIndex operatorMatrix =
    getOperatorMatrixElement (numberRowIndex * 2) (numberColumnIndex - 1) operatorMatrix

getBottomOperator :: Int -> Int -> OperatorMatrix -> Operator
getBottomOperator numberRowIndex numberColumnIndex operatorMatrix =
    if numberRowIndex >= getNRowsMatrix operatorMatrix then
      '|'
    else
      getOperatorMatrixElement (numberRowIndex * 2 + 1) numberColumnIndex operatorMatrix

getTopOperator :: Int -> Int -> OperatorMatrix -> Operator
getTopOperator 0 _ _ = '|'
getTopOperator numberRowIndex numberColumnIndex operatorMatrix =
      getOperatorMatrixElement (numberRowIndex * 2 - 1) numberColumnIndex operatorMatrix

clearRightOperator :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearRightOperator numberRowIndex numberColumnIndex = setMatrixElement (numberRowIndex * 2) numberColumnIndex '|'

clearLeftOperator :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearLeftOperator _ 0 operatorMatrix = operatorMatrix
clearLeftOperator numberRowIndex numberColumnIndex operatorMatrix = setMatrixElement (numberRowIndex * 2) (numberColumnIndex - 1) '|' operatorMatrix

clearBottomOperator :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearBottomOperator numberRowIndex numberColumnIndex operatorMatrix =
    if numberRowIndex >= getNRowsMatrix operatorMatrix then
        operatorMatrix
    else
        setMatrixElement (numberRowIndex * 2 + 1) numberColumnIndex '|' operatorMatrix

clearTopOperator :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearTopOperator 0 _ operatorMatrix = operatorMatrix
clearTopOperator numberRowIndex numberColumnIndex operatorMatrix = setMatrixElement (numberRowIndex * 2 - 1) numberColumnIndex '|' operatorMatrix

-- quando é oficialmente anotado o menor número de alguma caixa, é preciso
-- limpar os operadores adjacentes ao index do número anotado
clearAllOperatorsNextToNumber :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearAllOperatorsNextToNumber numberRowIndex numberColumnIndex operatorMatrix = do
    let clearRight = clearRightOperator numberRowIndex numberColumnIndex operatorMatrix
    let clearLeft = clearLeftOperator numberRowIndex numberColumnIndex clearRight
    let clearTop = clearTopOperator numberRowIndex numberColumnIndex clearLeft
    clearBottomOperator numberRowIndex numberColumnIndex clearTop

-- Checa se elemento [row, column] da matriz de números é menor que todos os seus vizinhos
-- (todos seus operadores adjacentes são "elemento é menor que vizinho")
checkIfSmallerThanEveryNeighbor :: Int -> Int -> OperatorMatrix -> Bool
checkIfSmallerThanEveryNeighbor row column operatorMatrix = do
    let rightOperator = getRightOperator row column operatorMatrix
    let leftOperator = getLeftOperator row column operatorMatrix
    let topOperator = getTopOperator row column operatorMatrix
    let bottomOperator = getBottomOperator row column operatorMatrix

    not ((rightOperator == '|') && (leftOperator == '|') && (topOperator == '|') && (bottomOperator == '|')) &&
        not ((rightOperator == '>') || (leftOperator == '<') || (topOperator == '^') || (bottomOperator == 'v'))

markAllElements :: OperatorMatrix -> Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markAllElements [] _ _ _ = []
markAllElements _ _ _ [] = []
markAllElements operatorMatrix numberRowIndex numberColumnIndex markingsMatrix
    | numberRowIndex > getNRowsMatrix markingsMatrix = markingsMatrix
    | numberColumnIndex > getNColumnsMatrix markingsMatrix =
        markAllElements operatorMatrix (numberRowIndex + 1) 0 markingsMatrix
    | otherwise = do
        let result = checkIfSmallerThanEveryNeighbor numberRowIndex numberColumnIndex operatorMatrix
        if result then do 
                let updatedMarkingsMatrix = markMatrix numberRowIndex numberColumnIndex markingsMatrix
                markAllElements operatorMatrix numberRowIndex (numberColumnIndex + 1) updatedMarkingsMatrix
            else
                markAllElements operatorMatrix numberRowIndex (numberColumnIndex + 1) markingsMatrix

-- matriz de marcações já pronta
setDefinitiveNumber :: MarkingsMatrix -> NumbersMatrix -> Int -> (NumbersMatrix, (Int, Int))
setDefinitiveNumber [] _ _ = ([], (-1,-1))
setDefinitiveNumber _ [] _ = ([], (-1,-1))
setDefinitiveNumber markingsMatrix numbersMatrix numberToSet = do
    let (boxIndex, markingColumnInsideBox) = checkAllBoxes markingsMatrix
    if boxIndex >= 0 && markingColumnInsideBox >= 0 then do
        let matrixOrder = getNColumnsMatrix markingsMatrix
        let (numberRow, numberColumn) = getCorrectIndex (boxIndex, markingColumnInsideBox) matrixOrder
        (setMatrixElement numberRow numberColumn numberToSet numbersMatrix, (numberRow, numberColumn))
    else
        (numbersMatrix, (-1,-1))


createAndMarkMarkingsMatrix :: NumbersMatrix -> OperatorMatrix -> MarkingsMatrix
createAndMarkMarkingsMatrix numbersMatrix operatorMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    markAllElements operatorMatrix 0 0 markingsMatrix

clearOperatorsAndMarkings :: (Int, Int) -> OperatorMatrix -> MarkingsMatrix -> (OperatorMatrix, MarkingsMatrix)
clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix = do
    let clearedOperatorMatrix = clearAllOperatorsNextToNumber numberRowIndex numberColumnIndex operatorMatrix
    let clearedMarkingsMatrix = clearRowAndColumn numberRowIndex numberColumnIndex markingsMatrix
    (clearedOperatorMatrix, clearedMarkingsMatrix)

-- assume que é o começo da iteração do número em questão, então cria nova matriz de marcações do zero
setAllSmallerNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllSmallerNumbers numbersMatrix operatorMatrix numberToSet = do
    let markingsMatrix = createAndMarkMarkingsMatrix numbersMatrix operatorMatrix
    setAllSmallerNumbers2 markingsMatrix numbersMatrix operatorMatrix numberToSet

-- matriz de marcações já pronta
setAllSmallerNumbers2 :: MarkingsMatrix -> NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllSmallerNumbers2 markingsMatrix numbersMatrix operatorMatrix numberToSet = do
    let (newNumbersMatrix, (numberRowIndex, numberColumnIndex)) = setDefinitiveNumber markingsMatrix numbersMatrix numberToSet
    if numberRowIndex < 0 then
        (newNumbersMatrix, operatorMatrix)
    else do
        let (clearedOperatorMatrix, clearedMarkingsMatrix) = 
                clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix
        setAllSmallerNumbers2 clearedMarkingsMatrix newNumbersMatrix clearedOperatorMatrix numberToSet


setAllNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> NumbersMatrix
setAllNumbers numbersMatrix operatorMatrix numberToSet =
    if numberToSet > getNColumnsMatrix numbersMatrix then
        numbersMatrix
    else do
        let (newNumbersMatrix, newOperatorMatrix)= setAllSmallerNumbers numbersMatrix operatorMatrix numberToSet
        setAllNumbers newNumbersMatrix newOperatorMatrix (numberToSet+1)
        

