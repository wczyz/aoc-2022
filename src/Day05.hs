module Day05 (solve) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

newtype Stack = Stack [Char]

type Stacks = Map.Map Int Stack

newtype Move = Move (Int, Int, Int)

tuplify :: [a] -> (a, a, a)
tuplify [x, y, z] = (x, y, z)

applyMove :: ([Char] -> [Char]) -> Stacks -> Move -> Stacks
applyMove order stacks (Move (amount, from, to)) =
    let Stack fromElements = fromJust $ Map.lookup from stacks
        Stack toElements = fromJust $ Map.lookup to stacks
        (elements, newFromElements) = splitAt amount fromElements
        newToElements = order elements ++ toElements
     in Map.insert to (Stack newToElements) $
            Map.insert
                from
                (Stack newFromElements)
                stacks

getTopLetters :: ([Char] -> [Char]) -> Stacks -> [Move] -> String
getTopLetters order stacks moves = Map.foldl (\s (Stack xs) -> s ++ [head xs]) "" newStacks
  where
    newStacks = foldl (applyMove order) stacks moves

part1 :: Stacks -> [Move] -> String
part1 = getTopLetters reverse

part2 :: Stacks -> [Move] -> String
part2 = getTopLetters id

solve :: String -> IO ()
solve input = do
    let [stackInput, movesInput] = splitOn "\n\n" input
        stackLines = init $ lines stackInput

        getElements :: String -> [String]
        getElements xs = case xs of
            [] -> []
            (x : _) -> let (element, rest) = splitAt 3 xs in element : getElements (drop 1 rest)

        parseElement :: String -> [Char]
        parseElement ['[', x, ']'] = [x]
        parseElement _ = []

        parseStackLine :: String -> [Stack]
        parseStackLine line = map (Stack . parseElement) elements
          where
            elements = getElements line

        stacks :: Stacks
        stacks = Map.fromList $ zip [1 ..] stackList
          where
            combineStacks (Stack xs, Stack ys) = Stack (xs ++ ys)
            accumulateStacks :: [Stack] -> String -> [Stack]
            accumulateStacks stacks s = zipWith (curry combineStacks) stacks (parseStackLine s)

            stackList :: [Stack]
            stackList = foldl accumulateStacks (repeat (Stack [])) stackLines

        parseMove :: String -> Move
        parseMove s = Move . tuplify $ getInts s
          where
            getInts :: String -> [Int]
            getInts s@(x : xs)
                | isDigit x = let (element, rest) = span isDigit s in read element : getInts rest
                | otherwise = getInts $ dropWhile (not . isDigit) s
            getInts [] = []

        moves :: [Move]
        moves = map parseMove $ lines movesInput

    putStrLn "-- Part 1 -- " >> putStrLn (part1 stacks moves)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 stacks moves)
