{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use String" #-}
{-# HLINT ignore "Reduce duplication" #-}
{-# HLINT ignore "Use guards" #-}
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

-- Checa se elemento [row, column] da matriz de números é maior que todos os seus vizinhos
-- (todos seus operadores adjacentes são "elemento é maior que vizinho")
checkIfBiggerThanEveryNeighbor :: Int -> Int -> OperatorMatrix -> Bool
checkIfBiggerThanEveryNeighbor row column operatorMatrix = do
    let rightOperator = getRightOperator row column operatorMatrix
    let leftOperator = getLeftOperator row column operatorMatrix
    let topOperator = getTopOperator row column operatorMatrix
    let bottomOperator = getBottomOperator row column operatorMatrix

    not ((rightOperator == '|') && (leftOperator == '|') && (topOperator == '|') && (bottomOperator == '|')) &&
        not ((rightOperator == '<') || (leftOperator == '>') || (topOperator == 'v') || (bottomOperator == '^'))

-- Marca todos os elementos menores a partir dos operadores:
-- Matriz de comparação -> linha a ser preenchida -> coluna a ser preenchida -> matriz de marcação -> matriz com novas marcações
markAllSmallerElements :: OperatorMatrix -> Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markAllSmallerElements [] _ _ _ = []
markAllSmallerElements _ _ _ [] = []
markAllSmallerElements operatorMatrix numberRowIndex numberColumnIndex markingsMatrix
    | numberRowIndex > getNRowsMatrix markingsMatrix = markingsMatrix
    | numberColumnIndex > getNColumnsMatrix markingsMatrix =
        markAllSmallerElements operatorMatrix (numberRowIndex + 1) 0 markingsMatrix
    | otherwise = do
        let result = checkIfSmallerThanEveryNeighbor numberRowIndex numberColumnIndex operatorMatrix
        if result then do 
                let updatedMarkingsMatrix = markMatrix numberRowIndex numberColumnIndex markingsMatrix
                markAllSmallerElements operatorMatrix numberRowIndex (numberColumnIndex + 1) updatedMarkingsMatrix
            else
                markAllSmallerElements operatorMatrix numberRowIndex (numberColumnIndex + 1) markingsMatrix

-- Marca todos os elementos maiores a partir dos operadores:
-- Matriz de comparação -> linha a ser preenchida -> coluna a ser preenchida -> matriz de marcação -> matriz com novas marcações
markAllBiggerElements :: OperatorMatrix -> Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markAllBiggerElements [] _ _ _ = []
markAllBiggerElements _ _ _ [] = []
markAllBiggerElements operatorMatrix numberRowIndex numberColumnIndex markingsMatrix
    | numberRowIndex > getNRowsMatrix markingsMatrix = markingsMatrix
    | numberColumnIndex > getNColumnsMatrix markingsMatrix =
        markAllBiggerElements operatorMatrix (numberRowIndex + 1) 0 markingsMatrix
    | otherwise = do
        let result = checkIfBiggerThanEveryNeighbor numberRowIndex numberColumnIndex operatorMatrix
        if result then do 
                let updatedMarkingsMatrix = markMatrix numberRowIndex numberColumnIndex markingsMatrix
                markAllBiggerElements operatorMatrix numberRowIndex (numberColumnIndex + 1) updatedMarkingsMatrix
            else
                markAllBiggerElements operatorMatrix numberRowIndex (numberColumnIndex + 1) markingsMatrix

-- Marca um número na matriz de números:
-- Matriz de marcação -> matriz de números -> valor a ser preenchido -> (matriz de números atualizada, (coordenadas do preenchimento))
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

-- Cria matriz de marcação dos menores números de acordo com os comparadores:
-- Matriz de números -> matriz de operadores -> matriz de marcação
createAndMarkSmallerMarkingsMatrix :: NumbersMatrix -> OperatorMatrix -> MarkingsMatrix
createAndMarkSmallerMarkingsMatrix numbersMatrix operatorMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    markAllSmallerElements operatorMatrix 0 0 markingsMatrix

-- Cria matriz de marcação dos maiores números de acordo com os comparadores:
-- Matriz de números -> matriz de operadores -> matriz de marcação
createAndMarkBiggerMarkingsMatrix :: NumbersMatrix -> OperatorMatrix -> MarkingsMatrix
createAndMarkBiggerMarkingsMatrix numbersMatrix operatorMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    markAllBiggerElements operatorMatrix 0 0 markingsMatrix

-- Limpa matriz de marcações e operadores baseado das coordenadas passadas quando preenche um número:
-- (linha, coluna) -> matriz de operadores -> matriz de marcação -> (matriz de operadores limpa, matriz de marcação limpa)
clearOperatorsAndMarkings :: (Int, Int) -> OperatorMatrix -> MarkingsMatrix -> (OperatorMatrix, MarkingsMatrix)
clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix = do
    let clearedOperatorMatrix = clearAllOperatorsNextToNumber numberRowIndex numberColumnIndex operatorMatrix
    let clearedMarkingsMatrix = clearRowAndColumn numberRowIndex numberColumnIndex markingsMatrix
    (clearedOperatorMatrix, clearedMarkingsMatrix)

-- Assume que é o começo da iteração do número em questão, então cria nova matriz de marcações do zero
-- Matriz de números -> matriz de operadores -> valor a ser preenchido -> (Matriz de números preenchida, matriz de operadores com menos comparadores)
setAllSmallerNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllSmallerNumbers numbersMatrix operatorMatrix numberToSet = do
    let markingsMatrix = createAndMarkSmallerMarkingsMatrix numbersMatrix operatorMatrix
    setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet

-- Assume que é o começo da iteração do número em questão, então cria nova matriz de marcações do zero
-- Matriz de números -> matriz de operadores -> valor a ser preenchido -> (Matriz de números preenchida, matriz de operadores com menos comparadores)
setAllBiggerNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllBiggerNumbers numbersMatrix operatorMatrix numberToSet = do
    let markingsMatrix = createAndMarkBiggerMarkingsMatrix numbersMatrix operatorMatrix
    setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet

-- Preenche os números de mesmo valor a partir da matriz de marcação:
-- Matriz de marcação -> matriz de números -> matriz de operadores -> valor a ser preenchido -> (Matriz de números preenchida, matriz de operadores com menos comparadores)
setAllNumbersFromIteration :: MarkingsMatrix -> NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet = do
    let (newNumbersMatrix, (numberRowIndex, numberColumnIndex)) = setDefinitiveNumber markingsMatrix numbersMatrix numberToSet
    if numberRowIndex < 0 then
        (newNumbersMatrix, operatorMatrix)
    else do
        let (clearedOperatorMatrix, clearedMarkingsMatrix) = 
                clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix
        setAllNumbersFromIteration clearedMarkingsMatrix newNumbersMatrix clearedOperatorMatrix numberToSet

-- Preenche todos os números a partir dos menores:
-- Matriz de números -> matriz de operadores -> valor utilizado para preencher -> Matriz resultado
setAllNumbersStartingFromSmallest :: NumbersMatrix -> OperatorMatrix -> Int -> NumbersMatrix
setAllNumbersStartingFromSmallest numbersMatrix operatorMatrix numberToSet = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    if numberToSet > matrixOrder then
        numbersMatrix
    else if operatorMatrix == fillNewMatrix (2 * matrixOrder - 1) matrixOrder '|' then
        if countMatrixElementOcurrences (matrixOrder-1) numbersMatrix == matrixOrder then
            fillZerosWithBiggestNumber numbersMatrix
        else
            numbersMatrix
    else do
        let (newNumbersMatrix, newOperatorMatrix)= setAllSmallerNumbers numbersMatrix operatorMatrix numberToSet
        if newNumbersMatrix == numbersMatrix then
            numbersMatrix
        else
            setAllNumbersStartingFromSmallest newNumbersMatrix newOperatorMatrix (numberToSet+1)

-- Preenche todos os números a partir dos maiores:
-- Matriz de números -> matriz de operadores -> valor utilizado para preencher -> Matriz resultado
setAllNumbersStartingFromBiggest :: NumbersMatrix -> OperatorMatrix -> Int -> NumbersMatrix
setAllNumbersStartingFromBiggest numbersMatrix operatorMatrix numberToSet = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    if numberToSet > matrixOrder || numberToSet < 1 || (operatorMatrix == fillNewMatrix (2 * matrixOrder - 1) matrixOrder '|') then
        numbersMatrix
    else do
        let (newNumbersMatrix, newOperatorMatrix)= setAllBiggerNumbers numbersMatrix operatorMatrix numberToSet
        if newNumbersMatrix == numbersMatrix then
            numbersMatrix
        else
            setAllNumbersStartingFromBiggest newNumbersMatrix newOperatorMatrix (numberToSet-1)

-- Preenche o resto dos números nulos com o maior número:
-- Matriz de números com nulos -> matriz de números sem nulos
fillZerosWithBiggestNumber :: NumbersMatrix -> NumbersMatrix
fillZerosWithBiggestNumber [] = []
fillZerosWithBiggestNumber numbersMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let (zeroIndexRow, zeroIndexColumn) = getElementIndexMatrix 0 numbersMatrix
    if zeroIndexRow >= 0 && zeroIndexRow < matrixOrder && zeroIndexColumn >= 0 && zeroIndexColumn < matrixOrder then do
        let newNumbersMatrix = setMatrixElement zeroIndexRow zeroIndexColumn matrixOrder numbersMatrix
        fillZerosWithBiggestNumber newNumbersMatrix
    else
        numbersMatrix

-- Elimina os comparadores da matriz de comparadores baseado nos elementos não nulos da matriz de números:
-- Linha -> Coluna -> matriz de números -> matriz de operadores -> matriz de operadores enxugada
getOperatorMatrixFromNumbersMatrix :: Int -> Int -> NumbersMatrix -> OperatorMatrix -> OperatorMatrix
getOperatorMatrixFromNumbersMatrix _ _ [] _ = []
getOperatorMatrixFromNumbersMatrix _ _ _ [] = []
getOperatorMatrixFromNumbersMatrix row column numbersMatrix completeOperatorMatrix =
    if row >= getNRowsMatrix completeOperatorMatrix then
        completeOperatorMatrix
    else if column >= getNColumnsMatrix completeOperatorMatrix then
        getOperatorMatrixFromNumbersMatrix (row+1) column numbersMatrix completeOperatorMatrix
    else if getMatrixElement row column numbersMatrix /= 0 then do
        let clearedOperatorMatrix = clearAllOperatorsNextToNumber row column completeOperatorMatrix
        getOperatorMatrixFromNumbersMatrix row (column+1) numbersMatrix clearedOperatorMatrix
    else
        getOperatorMatrixFromNumbersMatrix row (column+1) numbersMatrix completeOperatorMatrix

-- Soluciona o tabuleiro de acordo com a matriz de operadores
-- Por enquanto só parece funcionar 100% para tabuleiros 4x4
-- Caso contrário, tende a repetir alguns números, seja na mesma região, linha ou coluna, ou faltarem números
solvePuzzle :: OperatorMatrix -> NumbersMatrix
solvePuzzle [] = []
solvePuzzle operatorMatrix = do
    let matrixOrder = getNColumnsMatrix operatorMatrix
    let emptyNumbersMatrix = fillNewMatrix matrixOrder matrixOrder 0
    let numbersMatrixWithSmallerNumbers = setAllNumbersStartingFromSmallest emptyNumbersMatrix operatorMatrix 1
    if matrixContainsElement 0 numbersMatrixWithSmallerNumbers then do
        let updatedOperatorMatrix = getOperatorMatrixFromNumbersMatrix 0 0 numbersMatrixWithSmallerNumbers operatorMatrix
        if updatedOperatorMatrix == fillNewMatrix (2 * matrixOrder - 1) matrixOrder '|' then
            setAllNumbersStartingFromBiggest numbersMatrixWithSmallerNumbers operatorMatrix matrixOrder
        else

            setAllNumbersStartingFromBiggest numbersMatrixWithSmallerNumbers updatedOperatorMatrix matrixOrder
    else
        numbersMatrixWithSmallerNumbers