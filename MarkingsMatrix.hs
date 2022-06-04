{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}

module MarkingsMatrix where

import Matrix

type MarkingsArray = [Bool]
type MarkingsMatrix = [MarkingsArray]

-- insere ordem da matriz para descobrir tamanho das "caixas" de comparadores
boxSize :: Int -> [Int]
boxSize 0 = [0, 0]
boxSize 1 = [1, 1]
boxSize 4 = [2, 2]
boxSize 6 = [3, 2]
boxSize 9 = [3, 3]

-- TODO
-- faz com que cada linha seja equivalente a uma caixa
-- assume que a matriz é quadrada
-- boxes :: MarkingsArray -> MarkingsMatrix
-- boxes (a:b) =
--     let boxsize = boxSize getNColumnsMatrix (a:b)

splitRowPerBoxes :: Int -> Int -> [t] -> [[t]]
splitRowPerBoxes _ 0 _ = []
splitRowPerBoxes 0 _ row = []
splitRowPerBoxes boxWidth boxesPerRow row =
    --boxesPerRow = getArrayLength (a:b) / boxWidth
    [splitArray 0 boxWidth row]++splitRowPerBoxes boxWidth k newRow
    where newRow = splitArray boxWidth rowLength row
          k = boxesPerRow-1
          rowLength = getArrayLength row

markMatrix :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
markMatrix row column = setMatrixElement row column True

-- quando é oficialmente anotado o menor número de alguma caixa, é preciso 
-- limpar as marcações da linha e coluna do elemento
clearRowAndColumn :: Int -> Int -> MarkingsMatrix -> MarkingsMatrix
clearRowAndColumn _ _ [] = []
clearRowAndColumn row column matrix = do
    let clearArray = fillNewArray (getNColumnsMatrix matrix) False
    let partiallyClearMatrix = setMatrixRow row clearArray matrix
    setMatrixColumn column clearArray partiallyClearMatrix

printMarkingsMatrix :: MarkingsMatrix -> IO ()
printMarkingsMatrix [] = putStrLn []
printMarkingsMatrix (a:b) = do
    putStrLn(markingsArrayString a)
    printMarkingsMatrix b

markingsMatrixString :: MarkingsMatrix -> String
markingsMatrixString [] = []
markingsMatrixString (a:b) = markingsArrayString a ++ markingsMatrixString b

markingsArrayString :: MarkingsArray -> String
markingsArrayString [] = []
markingsArrayString (a:b) = show a ++ " " ++ markingsArrayString b