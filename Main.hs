import Matrix
import MarkingsMatrix
import PuzzleSolver
import Backtracker

-- correctResult :: [[Int]] -- esses dois resolve
-- correctResult = [[1, 2, 4, 3],
--                  [4, 3, 1, 2],
--                  [3, 4, 2, 1],
--                  [2, 1, 3, 4]]

-- operatorMatrix :: OperatorMatrix
-- operatorMatrix = [['<', '|', '>', '|'],
--                   ['^', '^', 'v', 'v'],
--                   ['>', '|', '<', '|'],
--                   ['|', '|', '|', '|'],
--                   ['<', '|', '>', '|'],
--                   ['v', 'v', '^', '^'],
--                   ['>', '|', '<', '|']]

correctResult4 :: [[Int]]
correctResult4 = [[2, 3, 1, 4], 
                  [1, 4, 3, 2], 
                  [4, 1, 2, 3], 
                  [3, 2, 4, 1]] 


operatorMatrix4 = [['<', '|', '<', '|'], 
                   ['v', '^', '^', 'v'], 
                   ['<', '|', '>', '|'], 
                   ['|', '|', '|', '|'], 
                   ['>', '|', '<', '|'], 
                   ['v', '^', '^', 'v'], 
                   ['>', '|', '>', '|']]

operatorMatrix9 = [
  ['<', '>', '|', '<', '<', '|', '>', '>', '|'],
  ['v', '^', '^', 'v', 'v', 'v', '^', 'v', '^'],
  ['<', '<', '|', '<', '>', '|', '>', '<', '|'],
  ['^', '^', '^', '^', 'v', 'v', 'v', 'v', 'v'],
  ['<', '<', '|', '<', '>', '|', '>', '<', '|'],
  ['|', '|', '|', '|', '|', '|', '|', '|', '|'],
  ['>', '>', '|', '>', '<', '|', '<', '>', '|'],
  ['v', '^', '^', 'v', 'v', 'v', '^', 'v', '^'],
  ['>', '<', '|', '>', '<', '|', '>', '>', '|'],
  ['v', '^', '^', '^', '^', 'v', 'v', 'v', 'v'],
  ['<', '>', '|', '<', '>', '|', '>', '<', '|'],
  ['|', '|', '|', '|', '|', '|', '|', '|', '|'],
  ['<', '>', '|', '>', '>', '|', '<', '>', '|'],
  ['v', 'v', 'v', '^', 'v', 'v', '^', '^', '^'],
  ['>', '<', '|', '>', '<', '|', '<', '<', '|'],
  ['^', '^', 'v', 'v', 'v', '^', 'v', '^', 'v'],
  ['<', '>', '|', '<', '<', '|', '<', '>', '|']]

operatorMatrix6 = [
  ['<', '|', '>', '|', '>', '|'],
  ['^', 'v', '^', '^', '^', 'v'],
  ['>', '|', '>', '|', '>', '|'],
  ['v', 'v', 'v', '^', '^', '^'],
  ['<', '|', '<', '|', '<', '|'],
  ['|', '|', '|', '|', '|', '|'],
  ['<', '|', '<', '|', '<', '|'],
  ['^', '^', 'v', 'v', '^', 'v'],
  ['>', '|', '<', '|', '<', '|'],
  ['v', 'v', '^', '^', '^', 'v'],
  ['>', '|', '<', '|', '>', '|']]

-- markingsMatrix :: MarkingsMatrix
-- markingsMatrix = fillNewMatrix 4 4 False

numbersMatrix4 :: [[Int]]
numbersMatrix4 = fillNewMatrix 4 4 0

numbersMatrix6 :: [[Int]]
numbersMatrix6 = fillNewMatrix 6 6 0

numbersMatrix9 :: [[Int]]
numbersMatrix9 = fillNewMatrix 9 9 0

-- numbersMatrix :: [[Int]]
-- numbersMatrix = [[0, 0, 1, 0],
--                  [1, 0, 0, 0],
--                  [0, 1, 0, 1],
--                  [0, 0, 0, 0]]

-- markingsMatrix :: [[Bool]]
-- markingsMatrix = [[False, False, True, False],
--                  [True, False, False, False],
--                  [False, True, False, False],
--                  [False, False, False, False]]

main :: IO ()
main = do

    putStrLn " "
    -- let matrixOrder = getNColumnsMatrix numbersMatrix

    -- let markingsMatrix = createAndMarkMarkingsMatrix numbersMatrix operatorMatrix
    -- printMatrix markingsMatrix
    -- putStrLn "Iteração 1"
    -- let (newNumbersMatrix, (numberRowIndex, numberColumnIndex)) = setDefinitiveNumber markingsMatrix numbersMatrix 1
    -- print (numberRowIndex, numberColumnIndex)
    -- if numberRowIndex < 0 then do
    --     putStrLn "cabo"
    --     printMatrix newNumbersMatrix
    -- else do
    --     let (clearedOperatorMatrix, clearedMarkingsMatrix) = 
    --             clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix
    --     printMatrix clearedMarkingsMatrix
    --     printMatrix newNumbersMatrix
    --     -- let boxRows = boxesAsRows clearedMarkingsMatrix
    --     -- print boxRows
    --     -- let indexCaixa = getCorrectMarkingIndex boxRows
    --     -- print(getCorrectIndex indexCaixa matrixOrder)
    -- --     putStrLn "Iteração 2"
        
    --     --let (newNumbersMatrix2, (numberRowIndex2, numberColumnIndex2)) = setDefinitiveNumber clearedMarkingsMatrix newNumbersMatrix 1
    --     let (boxIndex, markingColumnInsideBox) = checkAllBoxes clearedMarkingsMatrix -- retorna 0,2 quando deveria ser 1,0
    --     let matrixOrderr = getNColumnsMatrix clearedMarkingsMatrix
    --     print matrixOrderr
    --     print(boxIndex, markingColumnInsideBox)


        -- print (numberRowIndex2, numberColumnIndex2)
        -- if numberRowIndex2 < 0 then do
        --     putStrLn "cabo"
        --     printMatrix newNumbersMatrix2
        -- else do
        --     let (clearedOperatorMatrix2, clearedMarkingsMatrix2) = 
        --             clearOperatorsAndMarkings (numberRowIndex2, numberColumnIndex2) clearedOperatorMatrix clearedMarkingsMatrix
        
        --     -- printMatrix clearedOperatorMatrix
        --     printMatrix clearedMarkingsMatrix2
        --     printMatrix newNumbersMatrix2

    -- let indexMatrix = fillNewMatrix 4 4 False
    -- let markedIndexMatrix = markMatrix 1 0 indexMatrix
    -- let boxesIndexMatrix = boxesAsRows markedIndexMatrix
    -- print(getElementIndexMatrix True boxesIndexMatrix)

    -- printMatrix markingsMatrix
    -- printMatrix operatorMatrix
    -- let (newNumbersMatrix, (numberRowIndex, numberColumnIndex)) = setDefinitiveNumber markingsMatrix numbersMatrix 1
    -- printMatrix newNumbersMatrix
    -- print (numberRowIndex, numberColumnIndex)
    -- -- let (clearedOperatorMatrix, clearedMarkingsMatrix) = clearOperatorsAndMarkings (numberRowIndex, numberColumnIndex) operatorMatrix markingsMatrix
    -- printMatrix clearedOperatorMatrix
    -- printMatrix clearedMarkingsMatrix

    --printMatrix(fillZerosWithBiggestNumber numbersMatrix)
    let huhu1 = setAllNumbers numbersMatrix6 operatorMatrix6 1
    let yeye = getOperatorMatrixFromNumbersMatrix 0 0 huhu1 operatorMatrix6
    -- printMatrix yeye
    let huhu= setAllNumbersStartingFromBiggest huhu1 operatorMatrix6 6
    let huhu2= setAllNumbersStartingFromBiggest huhu1 yeye 6
    printMatrix huhu
    printMatrix huhu2
    print(countMatrixElementOcurrences 1 huhu)

    -- let (newNumbersMatrix, newOperatorMatrix)= setAllSmallerNumbers numbersMatrix operatorMatrix 1
    -- printMatrix newNumbersMatrix
    -- printMatrix newOperatorMatrix
    -- let (newNumbersMatrix2, newOperatorMatrix2)= setAllSmallerNumbers newNumbersMatrix newOperatorMatrix 2
    -- printMatrix newNumbersMatrix2
    -- printMatrix newOperatorMatrix2
    -- let (newNumbersMatrix3, newOperatorMatrix3)= setAllSmallerNumbers newNumbersMatrix2 newOperatorMatrix2 3
    -- printMatrix newNumbersMatrix3
    -- printMatrix newOperatorMatrix3
    -- let (newNumbersMatrix4, newOperatorMatrix4)= setAllSmallerNumbers newNumbersMatrix3 newOperatorMatrix3 4
    -- printMatrix newNumbersMatrix4
    -- printMatrix newOperatorMatrix4

        


