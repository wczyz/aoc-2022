module Day08 (solve) where

import Data.List (singleton, transpose)
import qualified Data.Set as Set

type Position = (Int, Int)

type Positions = Set.Set Position

data Direction = Row | Column

data Order = Normal | Reverse

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

nextPosition :: Direction -> Order -> Position -> Position
nextPosition direction order (i, j) =
    case direction of
        Row -> (i, next j)
        Column -> (next i, j)
  where
    next = case order of
        Normal -> (+ 1)
        Reverse -> (\x -> x - 1)

part1 :: [[Int]] -> String
part1 forest = show $ countVisible forest
  where
    helper :: Direction -> Order -> (Int, Position, Positions) -> Int -> (Int, Position, Positions)
    helper direction order (highest, position, positions) x =
        if x > highest
            then (x, nextPos, Set.insert position positions)
            else (highest, nextPos, positions)
      where
        nextPos = nextPosition direction order position

    countLeft :: Direction -> Int -> [Int] -> Positions -> Positions
    countLeft direction rowNumber row positions = thd3 $ foldl (helper direction Normal) (-1, pos, positions) row
      where
        pos = case direction of
            Row -> (rowNumber, 0)
            Column -> (0, rowNumber)

    countRight :: Direction -> Int -> [Int] -> Positions -> Positions
    countRight direction rowNumber row positions = thd3 $ foldr (flip $ helper Row Reverse) (-1, pos, positions) row
      where
        pos = case direction of
            Row -> (rowNumber, length row - 1)
            Column -> (length row - 1, rowNumber)

    countVisible :: [[Int]] -> Int
    countVisible forest = Set.size positions4
      where
        (_, positions) = foldl f (0, Set.empty) forest
        f (rowNumber, positions) row = (rowNumber + 1, countLeft Row rowNumber row positions)

        (_, positions2) = foldl g (0, positions) forest
        g (rowNumber, positions) row = (rowNumber + 1, countRight Row rowNumber row positions)

        transposedForest = transpose forest

        (_, positions3) = foldl h (0, positions2) transposedForest
        h (rowNumber, positions) row = (rowNumber + 1, countLeft Column rowNumber row positions)

        (_, positions4) = foldl i (0, Set.empty) transposedForest
        i (rowNumber, positions) row = (rowNumber + 1, countRight Column rowNumber row positions)

solve :: String -> IO ()
solve input = do
    let parseLine :: String -> [Int]
        parseLine = map (read . singleton)

        forest :: [[Int]]
        forest = map parseLine $ lines input

    putStrLn "-- Part 1 -- " >> putStrLn (part1 forest)

-- putStrLn "-- Part 2 -- " >> putStrLn (part2 dirMap)
