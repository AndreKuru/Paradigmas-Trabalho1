import Matrix
import MarkingsMatrix
import PuzzleSolver
import Backtracker

correctResult :: [[Int]]
correctResult = [[2, 3, 1, 4],
                 [1, 4, 3, 2],
                 [4, 1, 2, 3],
                 [3, 2, 4, 1]]

operatorMatrix :: OperatorMatrix
operatorMatrix = [['<', '|', '<', '|'],
                  ['v', '^', '^', 'v'],
                  ['<', '|', '>', '|'],
                  ['|', '|', '|', '|'],
                  ['>', '|', '<', '|'],
                  ['v', '^', '^', 'v'],
                  ['>', '|', '>', '|']]

-- markingsMatrix :: MarkingsMatrix
-- markingsMatrix = fillNewMatrix 4 4 False

-- numbersMatrix :: [[Int]]
-- numbersMatrix = fillNewMatrix 4 4 0

numbersMatrix :: [[Int]]
numbersMatrix = [[0, 0, 1, 0],
                 [1, 0, 0, 0],
                 [0, 1, 0, 1],
                 [0, 0, 0, 0]]

markingsMatrix :: [[Bool]]
markingsMatrix = [[False, False, True, False],
                 [True, False, False, False],
                 [False, True, False, False],
                 [False, False, False, False]]

main :: IO ()
main = do

    -- let matrixOrder = getNColumnsMatrix numbersMatrix
    -- let markingsMatrix = fillNewMatrix matrixOrder matrixOrder False
    putStrLn " "
    -- let markedMatrix = markAllElements operatorMatrix 0 0 markingsMatrix
    -- printMatrix markedMatrix

    let (newNumbersMatrix, [numberRowIndex, numberColumnIndex]) = setDefinitiveNumber markingsMatrix numbersMatrix 1
    if numberRowIndex < 0 then do
        putStrLn "cabo"
        printMatrix newNumbersMatrix
    else do
        printMatrix newNumbersMatrix
        


