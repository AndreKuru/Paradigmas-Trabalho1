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

-- quando ?? oficialmente anotado o menor n??mero de alguma caixa, ?? preciso
-- limpar os operadores adjacentes ao index do n??mero anotado
clearAllOperatorsNextToNumber :: Int -> Int -> OperatorMatrix -> OperatorMatrix
clearAllOperatorsNextToNumber numberRowIndex numberColumnIndex operatorMatrix = do
    let clearRight = clearRightOperator numberRowIndex numberColumnIndex operatorMatrix
    let clearLeft = clearLeftOperator numberRowIndex numberColumnIndex clearRight
    let clearTop = clearTopOperator numberRowIndex numberColumnIndex clearLeft
    clearBottomOperator numberRowIndex numberColumnIndex clearTop

-- Checa se elemento [row, column] da matriz de n??meros ?? menor que todos os seus vizinhos
-- (todos seus operadores adjacentes s??o "elemento ?? menor que vizinho")
checkIfSmallerThanEveryNeighbor :: Int -> Int -> OperatorMatrix -> Bool
checkIfSmallerThanEveryNeighbor row column operatorMatrix = do
    let rightOperator = getRightOperator row column operatorMatrix
    let leftOperator = getLeftOperator row column operatorMatrix
    let topOperator = getTopOperator row column operatorMatrix
    let bottomOperator = getBottomOperator row column operatorMatrix

    not ((rightOperator == '|') && (leftOperator == '|') && (topOperator == '|') && (bottomOperator == '|')) &&
        not ((rightOperator == '>') || (leftOperator == '<') || (topOperator == '^') || (bottomOperator == 'v'))

-- Checa se elemento [row, column] da matriz de n??meros ?? maior que todos os seus vizinhos
-- (todos seus operadores adjacentes s??o "elemento ?? maior que vizinho")
checkIfBiggerThanEveryNeighbor :: Int -> Int -> OperatorMatrix -> Bool
checkIfBiggerThanEveryNeighbor row column operatorMatrix = do
    let rightOperator = getRightOperator row column operatorMatrix
    let leftOperator = getLeftOperator row column operatorMatrix
    let topOperator = getTopOperator row column operatorMatrix
    let bottomOperator = getBottomOperator row column operatorMatrix

    not ((rightOperator == '|') && (leftOperator == '|') && (topOperator == '|') && (bottomOperator == '|')) &&
        not ((rightOperator == '<') || (leftOperator == '>') || (topOperator == 'v') || (bottomOperator == '^'))

-- Marca todos os elementos menores a partir dos operadores:
-- Matriz de compara????o -> linha a ser preenchida -> coluna a ser preenchida -> matriz de marca????o -> matriz com novas marca????es
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
-- Matriz de compara????o -> linha a ser preenchida -> coluna a ser preenchida -> matriz de marca????o -> matriz com novas marca????es
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

-- Marca um n??mero na matriz de n??meros:
-- Matriz de marca????o -> matriz de n??meros -> valor a ser preenchido -> (matriz de n??meros atualizada, (coordenadas do preenchimento))
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

-- Cria matriz de marca????o dos menores n??meros de acordo com os comparadores:
-- Matriz de n??meros -> matriz de operadores -> matriz de marca????o
createAndMarkSmallerMarkingsMatrix :: NumbersMatrix -> OperatorMatrix -> MarkingsMatrix
createAndMarkSmallerMarkingsMatrix numbersMatrix operatorMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    markAllSmallerElements operatorMatrix 0 0 markingsMatrix

-- Cria matriz de marca????o dos maiores n??meros de acordo com os comparadores:
-- Matriz de n??meros -> matriz de operadores -> matriz de marca????o
createAndMarkBiggerMarkingsMatrix :: NumbersMatrix -> OperatorMatrix -> MarkingsMatrix
createAndMarkBiggerMarkingsMatrix numbersMatrix operatorMatrix = do
    let matrixOrder = getNColumnsMatrix numbersMatrix
    let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    markAllBiggerElements operatorMatrix 0 0 markingsMatrix

-- Limpa matriz de marca????es e operadores baseado das coordenadas passadas quando preenche um n??mero:
-- (linha, coluna) -> matriz de operadores -> matriz de marca????o -> (matriz de operadores limpa, matriz de marca????o limpa)
clearOperatorsAndMarkings :: (Int, Int) -> OperatorMatrix -> MarkingsMatrix -> (OperatorMatrix, MarkingsMatrix)
clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix = do
    let clearedOperatorMatrix = clearAllOperatorsNextToNumber numberRowIndex numberColumnIndex operatorMatrix
    let clearedMarkingsMatrix = clearRowAndColumn numberRowIndex numberColumnIndex markingsMatrix
    (clearedOperatorMatrix, clearedMarkingsMatrix)

-- Assume que ?? o come??o da itera????o do n??mero em quest??o, ent??o cria nova matriz de marca????es do zero
-- Matriz de n??meros -> matriz de operadores -> valor a ser preenchido -> (Matriz de n??meros preenchida, matriz de operadores com menos comparadores)
setAllSmallerNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllSmallerNumbers numbersMatrix operatorMatrix numberToSet = do
    let markingsMatrix = createAndMarkSmallerMarkingsMatrix numbersMatrix operatorMatrix
    setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet

-- Assume que ?? o come??o da itera????o do n??mero em quest??o, ent??o cria nova matriz de marca????es do zero
-- Matriz de n??meros -> matriz de operadores -> valor a ser preenchido -> (Matriz de n??meros preenchida, matriz de operadores com menos comparadores)
setAllBiggerNumbers :: NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllBiggerNumbers numbersMatrix operatorMatrix numberToSet = do
    let markingsMatrix = createAndMarkBiggerMarkingsMatrix numbersMatrix operatorMatrix
    setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet

-- Preenche os n??meros de mesmo valor a partir da matriz de marca????o:
-- Matriz de marca????o -> matriz de n??meros -> matriz de operadores -> valor a ser preenchido -> (Matriz de n??meros preenchida, matriz de operadores com menos comparadores)
setAllNumbersFromIteration :: MarkingsMatrix -> NumbersMatrix -> OperatorMatrix -> Int -> (NumbersMatrix, OperatorMatrix)
setAllNumbersFromIteration markingsMatrix numbersMatrix operatorMatrix numberToSet = do
    let (newNumbersMatrix, (numberRowIndex, numberColumnIndex)) = setDefinitiveNumber markingsMatrix numbersMatrix numberToSet
    if numberRowIndex < 0 then
        (newNumbersMatrix, operatorMatrix)
    else do
        let (clearedOperatorMatrix, clearedMarkingsMatrix) = 
                clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix
        setAllNumbersFromIteration clearedMarkingsMatrix newNumbersMatrix clearedOperatorMatrix numberToSet

-- Preenche todos os n??meros a partir dos menores:
-- Matriz de n??meros -> matriz de operadores -> valor utilizado para preencher -> Matriz resultado
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

-- Preenche todos os n??meros a partir dos maiores:
-- Matriz de n??meros -> matriz de operadores -> valor utilizado para preencher -> Matriz resultado
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

-- Preenche o resto dos n??meros nulos com o maior n??mero:
-- Matriz de n??meros com nulos -> matriz de n??meros sem nulos
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

-- Elimina os comparadores da matriz de comparadores baseado nos elementos n??o nulos da matriz de n??meros:
-- Linha -> Coluna -> matriz de n??meros -> matriz de operadores -> matriz de operadores enxugada
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
-- Por enquanto s?? parece funcionar 100% para tabuleiros 4x4
-- Caso contr??rio, tende a repetir alguns n??meros, seja na mesma regi??o, linha ou coluna, ou faltarem n??meros
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