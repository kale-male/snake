module Main where

--import Lib
import           Control.Monad

sum2 :: String -> String -> Int
sum2 a b = read a + read b

getList :: IO [Int]
getList = do
  s <- getLine
  return $map read $words s

readMatrix :: IO [[Int]]
readMatrix = do
  n <- readLn
  replicateM n getList

sumLDiag :: [[Int]] -> Int
sumLDiag xs = sum $map diag [0 .. length xs - 1]
  where
    diag i = xs !! i !! i

sumRDiag :: [[Int]] -> Int
sumRDiag xs = sum $map diag [0 .. length xs - 1]
  where
    diag i = xs !! i !! (length xs - 1 - i)

main :: IO ()
main = do
  matr <- readMatrix
  print $abs $sumLDiag matr - sumRDiag matr
