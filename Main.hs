import Matrix
import MarkingsMatrix
import Solver

myMatrix :: Matrix
myMatrix = [[2, 3, 1, 4], 
            [1, 4, 3, 2], 
            [4, 1, 2, 3], 
            [3, 2, 4, 1]] 

myMatrixOperator = [['<', '|', '<', '|'], 
                    ['v', '^', '^', 'v'], 
                    ['<', '|', '>', '|'], 
                    ['|', '|', '|', '|'], 
                    ['>', '|', '<', '|'], 
                    ['v', '^', '^', 'v'], 
                    ['>', '|', '>', '|']]

main = do
    
    print (solveMatrix myMatrixOperator)
    -- let myMatrix2 = fillNewMatrix 7 5 False
    -- printMarkingsMatrix myMatrix2
    -- let myMatrix3 = markMatrix 2 2 myMatrix2
    -- let myMatrix4 = markMatrix 2 4 myMatrix3
    -- let myMatrix5 = markMatrix 0 2 myMatrix4
    -- let myMatrix6 = markMatrix 3 4 myMatrix5
    -- let myMatrix7 = markMatrix 0 1 myMatrix6
    -- printMarkingsMatrix myMatrix7
    -- printMarkingsMatrix(clearRowAndColumn 2 2 myMatrix7)
    -- print (fillNewArray 6 True)
