module Day01 (solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

newtype Elf = Elf [Int]

elfSum :: Elf -> Int
elfSum (Elf xs) = sum xs

part1 :: [Elf] -> String
part1 elfs = 
    let maxSum = maximum $ map elfSum elfs
    in show maxSum

part2 :: [Elf] -> String
part2 elfs = 
    let sortedSums = reverse . sort $ map elfSum elfs
        value = sum (take 3 sortedSums)
    in show value

solve :: String -> IO ()
solve input = do
    let elfData :: [[Int]]
        elfData = map (map read . lines) $ splitOn "\n\n" input
        elfs :: [Elf]
        elfs = map Elf elfData
    putStrLn "-- Part 1 -- " >> putStrLn (part1 elfs)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 elfs)
