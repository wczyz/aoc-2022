module Day04 (solve) where

import Data.Char (isDigit)

newtype Section = Section (Int, Int)
newtype Assignment = Assignment (Section, Section)

part1 :: [Assignment] -> String
part1 assignments =
    let sectionContains :: Section -> Section -> Bool
        sectionContains (Section (a, b)) (Section (c, d)) = a >= c && b <= d

        assignmentContains :: Assignment -> Bool
        assignmentContains (Assignment (s1, s2)) = sectionContains s1 s2 || sectionContains s2 s1
     in show . length $ filter assignmentContains assignments

part2 :: [Assignment] -> String
part2 assignments =
    let sectionOverlaps :: Section -> Section -> Bool
        sectionOverlaps (Section (a, b)) (Section (c, d)) = b >= c && a <= c

        assignmentOverlaps :: Assignment -> Bool
        assignmentOverlaps (Assignment (s1, s2)) = sectionOverlaps s1 s2 || sectionOverlaps s2 s1
     in show . length $ filter assignmentOverlaps assignments

solve :: String -> IO ()
solve input = do
    let parseSection :: String -> (Section, String)
        parseSection s = (Section (read firstNum, read secondNum), secondRest)
          where
            (firstNum, firstRest) = span isDigit s
            (secondNum, secondRest) = span isDigit $ drop 1 firstRest

        parseAssignment :: String -> Assignment
        parseAssignment s = Assignment (firstSection, secondSection)
          where
            (firstSection, rest) = parseSection s
            (secondSection, _) = parseSection $ drop 1 rest

        assignments :: [Assignment]
        assignments = map parseAssignment $ lines input

    putStrLn "-- Part 1 -- " >> putStrLn (part1 assignments)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 assignments)
