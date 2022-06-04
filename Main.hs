import Matrix
import MarkingsMatrix

myMatrix :: Matrix
myMatrix = [[1,2,5], [2,3,2], [2,4,8]]

main = do
    let myMatrix2 = fillNewMatrix 7 5 False
    printMarkingsMatrix myMatrix2
    let myMatrix3 = markMatrix 2 2 myMatrix2
    let myMatrix4 = markMatrix 2 4 myMatrix3
    let myMatrix5 = markMatrix 0 2 myMatrix4
    let myMatrix6 = markMatrix 3 4 myMatrix5
    let myMatrix7 = markMatrix 0 1 myMatrix6
    printMarkingsMatrix myMatrix7
    printMarkingsMatrix(clearRowAndColumn 2 2 myMatrix7)
    print(splitRowPerBoxes 2 4 [1, 2, 3, 4, 5, 6, 7, 8])
    -- print createEmptyMatrix 5 5
