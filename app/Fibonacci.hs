module Fibonacci where

import           Control.Monad


ones :: [Int]
ones = 1:ones

numbers :: Int -> [Int]
numbers i = i:numbers(i+1)

numbersBy :: Int -> Int -> [Int]
numbersBy i j = i:numbersBy (i+j) j

fib ::Int -> Int -> [Int]
fib i j = i:fib j (i+j) 

m = 10^8 + 7

fibMod ::Int -> Int -> [Int]
fibMod i j = i:fibMod j (mod (i+j) m) 

readLine :: IO [Int]
readLine = do
  n <- readLn
  replicateM n readLn

main :: IO ()
main = do
  list <- readLine
  let fibNums =  fibMod 0 1
  forM_ list $ \i -> do
    let num = fibNums !! i
    print num
--main = print $ take 10 $ fib 0 1
--main = print $ take 10 $ numbersBy 10 (-2)
--main = print $ take 10 $ numbers(-1)
--main = print $ take 10 ones