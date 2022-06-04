{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Evaluate" #-}

module MarkingsMatrix where

import Matrix

type MarkingsArray = [Bool]
type MarkingsMatrix = [MarkingsArray]

-- insere ordem da matriz para descobrir tamanho das "caixas" (ou regiões) de comparadores
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

-- TODO
-- divideRowPerBox :: Int -> Int -> MarkingsArray -> MarkingsMatrix
-- divideRowPerBox 0 _ row = []
-- divideRowPerBox 1 _ row = []
-- divideRowPerBox boxWidth (a:b) boxesPerRow=
--     if boxWidth >= getArrayLength row then
--         [row]
--     else
--         [a] ++ divideRowPerBox boxWidth-1 b 

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