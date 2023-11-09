module Main where

import qualified MyLib (someFunc)
import Data.List

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

type Grid = Matrix Value

type Matrix a = [Row a]

-- using a row for operations, either that from a column or a box, i'll use it as a row
type Row a = [a]

type Value = Char

empty = replicate 9 $ replicate 9 "."

rows :: Matrix a -> [Row a]
--rows m = m
--id is in std: id x = x
rows = id

--Property:: rows (rows m) = m
--Property: rows ° rows = id

cols:: Matrix a -> [Row a]
cols = transpose

--Property: cols ° cols = id

boxs :: Matrix a -> [Row a]
boxs m = map (getBox m) [0..((length m) -1)]
--Property: boxs ° boxs = id

getBox :: Matrix a -> Int -> Row a
getBox m boxIndex = getCols (map (m!!) rowIndices) colIndices
  where
     rowIndices = map fst boxIndices
     colIndices = map snd boxIndices
     boxIndices = boxToIndices boxIndex

getRows :: Matrix a -> [Int] -> [Row a]
getRows m rowIndices = map (getRow m) rowIndices

getRow :: Matrix a -> Int -> Row a
getRow (x:xs) n
  | n == 1 = x
  | otherwise = getRow xs (n-1)

getCols :: [Row a] -> [Int] -> Row a
getCols [] [] = []
getCols (r:rs) (n:ns) = (r!!n):(getCols rs ns)

boxToIndices :: Int -> [(Int,Int)]
boxToIndices boxIndex = [(i,j)|i<-boxToRowIndices boxIndex, j<-boxToColIndices boxIndex]

boxToRowIndices :: Int -> [Int]
boxToRowIndices n
  | 0<=n && n<=2 = [0,1,2]
  | 3<=n && n<=5 = [3,4,5]
  | otherwise    = [6,7,8]

boxToColIndices :: Int -> [Int]
boxToColIndices n
  | n==0 || n==3 || n==6 = [0,1,2]
  | n==1 || n==4 || n==7 = [3,4,5]
  | otherwise            = [6,7,8]

valid:: Grid -> Bool
valid g =
  all nodups (rows) &&
  all nodups (cols) &&
  all nodups (boxs)

easy  :: Grid
easy   = [
  "7356148.2",
  ".42973.61",
  "9.128.374",
  "28.3.9157",
  "413.57926",
  "5791.6438",
  "15.492.83",
  ".947382.5",
  "32856174."]

soln  :: Grid
soln   = [
  "735614892",
  "842973561",
  "961285374",
  "286349157",
  "413857926",
  "579126438",
  "157492683",
  "694738215",
  "328561749"]

nodups :: Eq a => [Row a] -> Bool
nodups [] = True
nodups (x:xs) = ((length $ nub x) == length x) && nodups xs
