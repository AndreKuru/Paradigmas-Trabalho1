import Matrix

myMatrix :: Matrix
myMatrix = [[1,2,5], [2,3,2], [2,4,8]]

main = do
    -- todo: criar funcao de printMatrixar matriz com linhas uma baixo da outra
    printMatrix myMatrix
    print(getMatrixRow 2 myMatrix)
    print(getNRowsMatrix myMatrix)
    print(getNColumnsMatrix myMatrix)
    print(getMatrixColumn 2 myMatrix)
    print(getMatrixElement 1 1 myMatrix)
    print(splitArray 1 4 [1,2,3,4,5])
    printMatrix(splitMatrixLines 1 2 myMatrix)
    printMatrix(setMatrixRow 2 [0, 0, 0] myMatrix)
    printMatrix(setMatrixElement 1 1 59 myMatrix)
    printMatrix (setMatrixColumn 3 [0,0,0] myMatrix)
