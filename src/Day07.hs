module Day07 (solve) where

import Data.Char (isDigit)
import Data.List (find, isPrefixOf, tails)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Command
    = ChangeDir String
    | List

data Input = Command Command | ResultDir String | ResultFile Integer String

data Element
    = Dir String -- Name
    | File Integer String -- Size Name

type DirMap = Map.Map [String] [Element]

addElement :: [String] -> Element -> DirMap -> DirMap
addElement path element dirMap = Map.insert path newElement dirMap
  where
    newElement = case Map.lookup path dirMap of
        Nothing -> [element]
        Just xs -> element : xs

parseLine :: String -> Input
parseLine s
    | "$ " `isPrefixOf` s = parseCommand $ drop 2 s
    | otherwise = parseResult s

parseCommand :: String -> Input
parseCommand s
    | "cd" `isPrefixOf` s = Command $ ChangeDir (drop 3 s)
    | "ls" `isPrefixOf` s = Command List

parseResult :: String -> Input
parseResult s
    | isDigit (head s) =
        let (sizeStr, rest) = span isDigit s
         in ResultFile (read sizeStr) (drop 1 rest)
    | otherwise = ResultDir (drop 4 s)

getDirSizes :: DirMap -> Map.Map [String] Integer
getDirSizes dirMap =
    let elementSize :: Element -> Integer
        elementSize (Dir _) = 0
        elementSize (File size _) = size

        sumElements :: [Element] -> Integer
        sumElements elements = sum $ map elementSize elements

        browse :: Map.Map [String] Integer -> [String] -> [Element] -> Map.Map [String] Integer
        browse sizesMap path elements = foldl (\m p -> Map.insertWith (+) p (sumElements elements) m) sizesMap (init $ tails path)
     in Map.foldlWithKey browse Map.empty dirMap

part1 :: DirMap -> String
part1 dirMap = show value
  where
    dirSizes = getDirSizes dirMap
    value = Map.foldl (\acc x -> if x <= 100000 then acc + x else acc) 0 dirSizes

part2 :: DirMap -> String
part2 dirMap =
    let totalSpace = 70000000
        neededSpace = 30000000
        dirSizes = getDirSizes dirMap
        takenSpace = fromJust $ Map.lookup ["/"] dirSizes
        searchSpace = neededSpace - (totalSpace - takenSpace)

        browse :: Integer -> Integer -> Integer
        browse old new = if new >= searchSpace && new < old then new else old
     in show $ Map.foldl browse takenSpace dirSizes

solve :: String -> IO ()
solve input = do
    let parsedInput :: [Input]
        parsedInput = map parseLine $ lines input

        buildMap :: ([String], DirMap) -> Input -> ([String], DirMap)
        buildMap (currentPath, dirMap) (Command (ChangeDir name)) =
            case name of
                ".." -> (drop 1 currentPath, dirMap)
                _ -> (name : currentPath, dirMap)
        buildMap (currentPath, dirMap) (ResultDir name) = (currentPath, addElement currentPath (Dir name) dirMap)
        buildMap (currentPath, dirMap) (ResultFile size name) = (currentPath, addElement currentPath (File size name) dirMap)
        buildMap x _ = x

        (_, dirMap) = foldl buildMap ([], Map.empty) parsedInput

    putStrLn "-- Part 1 -- " >> putStrLn (part1 dirMap)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 dirMap)
