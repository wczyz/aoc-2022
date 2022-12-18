module Day06 (solve) where

isUnique :: (Eq a) => [a] -> Bool
isUnique [] = True
isUnique (x : xs) = x `notElem` xs && isUnique xs

findPacketPosition :: String -> Int -> Int
findPacketPosition s uniqueSize = let (result, _) = helper s in result
  where
    helper :: String -> (Int, [Char])
    helper = foldl f (0, [])

    f :: (Int, [Char]) -> Char -> (Int, [Char])
    f (position, lastChars) c
        | length lastChars < uniqueSize = (position + 1, c : lastChars)
        | otherwise =
            if isUnique lastChars
                then (position, lastChars)
                else (position + 1, c : init lastChars)

part1 :: String -> String
part1 s = show $ findPacketPosition s 4

part2 :: String -> String
part2 s = show $ findPacketPosition s 14

solve :: String -> IO ()
solve input = do
    putStrLn "-- Part 1 -- " >> putStrLn (part1 input)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 input)
