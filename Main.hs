import Matrix
import MarkingsMatrix
import PuzzleSolver
import Backtracker

correctResult :: [[Int]]
correctResult = [[1, 2, 4, 3],
                 [4, 3, 1, 2],
                 [3, 4, 2, 1],
                 [2, 1, 3, 4]]

operatorMatrix :: OperatorMatrix
operatorMatrix = [['<', '|', '>', '|'],
                  ['^', '^', 'v', 'v'],
                  ['>', '|', '<', '|'],
                  ['|', '|', '|', '|'],
                  ['<', '|', '>', '|'],
                  ['v', 'v', '^', '^'],
                  ['>', '|', '<', '|']]

-- markingsMatrix :: MarkingsMatrix
-- markingsMatrix = fillNewMatrix 4 4 False

numbersMatrix :: [[Int]]
numbersMatrix = fillNewMatrix 4 4 0

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
    printMatrix(setAllNumbers numbersMatrix operatorMatrix 1)

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

        


