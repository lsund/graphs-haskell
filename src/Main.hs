

module Main where

import System.Random
import Control.Monad
import Data.List

lexiSort' xs n = 
    if n > length xs then 
        xs
    else
        let (zeroes, ones) = groupByBit n xs
        in lexiSort' zeroes (n + 1) ++ lexiSort' ones (n + 1)

lexiSort xs = lexiSort' xs 0


-- Group some strings according to their nth bit
groupByBit :: Int -> [[Int]] -> ([[Int]], [[Int]])
groupByBit n = partition (\x -> x !! n == 0)

genRandString :: Int -> IO [Int] 
genRandString n = replicateM n $  randomRIO (0 :: Int, 1 :: Int)

-- Generate n random strings of length m
genRandStrings :: Int -> Int -> IO [[Int]]
genRandStrings n m = replicateM n $ genRandString m

main :: IO ()
main = do
    num <- genRandString 10
    print num
    return ()

