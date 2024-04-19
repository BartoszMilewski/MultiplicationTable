module Main where

import System.Random (newStdGen, randomRs)
import Control.Monad (when)
import Data.List (group)
import Data.Bifunctor (bimap)

type Index = Int
type TestPair = (Int, Int)

-- mult.txt contains a list of numbers for testing multiplication
main :: IO ()
main = do
    text <- readFile "mult.txt"
    tests <- genTests $ getTestSet text
    loop tests
  where
    getTestSet :: String -> [Int]
    getTestSet text = map read $ words text
    loop :: [TestPair] -> IO ()
    loop (tst : tsts) = do
      more <- ask tst 2 -- two tries
      when more (loop tsts)

genTests :: [Int] -> IO [TestPair]
genTests nums = do
    let testSet = if length nums < 2 then [2..9] else nums
    gen <- newStdGen
    let hi = length testSet - 1
    let rs = randomRs (0, hi) gen
    let pairs = mkIdxPairs rs
    return $ mkTestPairs testSet pairs
  where
    mkIdxPairs :: [Index] -> [(Index, Index)]
    mkIdxPairs (a:b:rest) = (a, b) : mkIdxPairs rest
    mkTestPairs :: [Int] -> [(Index, Index)] -> [(Int, Int)]
    mkTestPairs testSet = map head . group . map (bimap (testSet !!) (testSet !!))

-- Pair to multiply, number of tries left
ask :: TestPair -> Int -> IO Bool
ask tst 0 = do
    putStrLn $ "The answer is: " ++ showMult tst True
    return True
ask tst n = do
    putStrLn $ showMult tst False
    putStr ": "
    str <- getLine
    if null str
    then return False
    else do
        yes <- check tst (read str)
        if yes
        then return True
        else ask tst (n - 1)

check :: TestPair -> Int -> IO Bool
check (x, y) z =
  if x * y == z
  then do
    putStrLn "**Correct**\n"
    return True
  else do
    putStrLn "**Incorrect**"
    return False

showMult :: TestPair -> Bool -> String
showMult (x, y) showAnswer =
   show x ++ " * " ++ show y ++ " = " ++
     (if showAnswer then show (x * y) else "?")
