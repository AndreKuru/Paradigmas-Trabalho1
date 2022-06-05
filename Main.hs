import Matrix
import MarkingsMatrix

myMatrix :: Matrix
myMatrix = [[1,2,5,6,4,5], [2,3,2,1,3,5], [2,4,8,9,3,5], [2,5,6,8,4,3],[1,5,7,3,4,5],[4,5,6,8,4,3]]

main = do
    printMatrix myMatrix
    -- let matrixOrder = getNColumnsMatrix myMatrix
    --let nRows = getNRowsMatrix myMatrix
    -- let boxwidth = boxWidth matrixOrder
    -- let boxheight = boxHeight matrixOrder
    -- let boxesPerRow = div matrixOrder boxwidth
    -- let boxSegments = splitMatrixPerBoxesPerLine boxwidth boxesPerRow myMatrix
    -- let orderedBoxSegments = mapColumns boxesPerRow matrixOrder boxSegments
    -- print orderedBoxSegments
    -- print(getNRowsMatrix orderedBoxSegments)
    -- let final = concatBoxSegments orderedBoxSegments boxheight boxesPerRow
    -- print final
    --print(concatMatrixLines 0 nRows myMatrix)
    print(boxesAsRows myMatrix)
