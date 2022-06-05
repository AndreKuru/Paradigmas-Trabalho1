import Matrix
import MarkingsMatrix

myMatrix :: MarkingsMatrix
myMatrix = [[False,False, True, True], [False,False, False, False], [True, True, False, False], [True,True, False, True]]

main = do
    printMatrix myMatrix
    let boxRows = boxesAsRows myMatrix
    print boxRows
    print(getCorrectMarkingIndex boxRows)
