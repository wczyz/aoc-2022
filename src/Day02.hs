{-# LANGUAGE FlexibleInstances #-}

module Day02 (solve) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Choice = Rock | Paper | Scissors deriving (Eq, Enum)

instance Ord Choice where
    compare Rock Paper = LT
    compare Paper Scissors = LT
    compare Scissors Rock = LT
    compare a b = if a == b then EQ else GT

choiceValue :: Choice -> Int
choiceValue c = case c of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

newtype Game = Game (Choice, Choice)

gameValue :: Game -> Int
gameValue (Game (a, b)) =
    choiceValue b
        + case compare a b of
            LT -> 6
            EQ -> 3
            GT -> 0

tuplify2 [x] = (x, x)
tuplify2 [x, y] = (x, y)

stringToChoice :: String -> Choice
stringToChoice s
    | s `elem` ["A", "X"] = Rock
    | s `elem` ["B", "Y"] = Paper
    | s `elem` ["C", "Z"] = Scissors

part1 :: [[String]] -> String
part1 gamesData =
    let games :: [Game]
        games = map (Game . tuplify2 . map stringToChoice) gamesData
     in show $ sum $ map gameValue games

data Result = Lose | Draw | Win

stringToResult :: String -> Result
stringToResult "X" = Lose
stringToResult "Y" = Draw
stringToResult "Z" = Win

infoToGame :: (Choice, Result) -> Game
infoToGame (x, Draw) = Game (x, x)
infoToGame (x, result) = Game (x, y)
  where
    f = case result of
        Win -> (> x)
        Lose -> (< x)
    y = fromJust $ find f [Rock .. Scissors]

part2 :: [[String]] -> String
part2 gamesData =
    let info :: [(Choice, Result)]
        info = map (bimap stringToChoice stringToResult . tuplify2) gamesData
        games = map infoToGame info
     in show $ sum $ map gameValue games

solve :: String -> IO ()
solve input = do
    let gamesData :: [[String]]
        gamesData = map (splitOn " ") (lines input)
    putStrLn "-- Part 1 -- " >> putStrLn (part1 gamesData)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 gamesData)
