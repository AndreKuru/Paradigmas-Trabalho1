import Matrix
import MarkingsMatrix

-- myMatrix :: MarkingsMatrix
-- myMatrix = [[False,False,False,False, False, False], [False,False,False,False, False, False], 
--     [False,False,True,False, False, False], [False,False,False,False, False, False],
--     [False,False,False,False, False, False],[False,False,False,False, False, False]]

myMatrix :: MarkingsMatrix
myMatrix = [[False,False,False,False], [False,False,False,False], 
    [False,True,False,False], [False,False,False,False]]

-- myMatrix :: Matrix
-- myMatrix = [[10,11,12,13,14,15],[16,17,18,19,20,21],[22,23,24,25,26,27],[28,29,30,31,32,33],[34,35,36,37,38,39],[40,41,42,43,44,45]]

main = do
    printMatrix myMatrix
    let boxRows = boxesAsRows myMatrix
    print boxRows
    print(getCorrectMarkingIndex boxRows)
    print(getElementIndexMatrix True myMatrix)
    print(boxesAsRows myMatrix)
    let idnex = checkAllBoxes myMatrix
    print idnex
    let [a,b] = idnex
    let matrixOrder = getNColumnsMatrix myMatrix
    let indexMatrix = fillNewMatrix matrixOrder matrixOrder False
    let nColumns =  b :: Int
    let markedIndexMatrix = markMatrix a nColumns indexMatrix
    printMatrix markedIndexMatrix
    let boxesIndexMatrix = boxesAsRows markedIndexMatrix
    printMatrix boxesIndexMatrix
    print(getElementIndexMatrix True boxesIndexMatrix)


    -- let matrixOrder = getNColumnsMatrix myMatrix
    -- let boxwidth = boxWidth matrixOrder
    -- let boxheight = boxHeight matrixOrder
    -- let boxesPerRow = div matrixOrder boxwidth
    -- let boxSegments = splitMatrixPerBoxesPerLine boxwidth boxesPerRow myMatrix
    -- print boxSegments
    -- let orderedBoxSegments = mapColumns boxesPerRow matrixOrder boxSegments
    -- print orderedBoxSegments
    -- let ableu = concatBoxSegments orderedBoxSegments boxheight
    -- print ableu
    -- let columnsPerRow = div matrixOrder boxheight
    -- print(mapColumns columnsPerRow matrixOrder ableu)


