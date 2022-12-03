module Day03 (solve) where
import Data.Char (ord, isAsciiLower, isAsciiUpper)
import qualified Data.Set as S (intersection, fromList, elemAt)

newtype Item = Item Char deriving (Eq, Ord)
newtype Rucksack = Rucksack [Item]

itemValue :: Item -> Int
itemValue (Item c) 
    | isAsciiLower c = ord c - ord 'a' + 1
    | isAsciiUpper c = ord c - ord 'A' + 27

part1 :: [Rucksack] -> String
part1 rucksacks = 
    let compartmentSize (Rucksack items) = length items `div` 2
        takeFirst r@(Rucksack items) = take (compartmentSize r) items
        takeSecond r@(Rucksack items) = drop (compartmentSize r) items

        findCommon :: Rucksack -> Item
        findCommon rucksack = S.elemAt 0 $ firstSet `S.intersection` secondSet
            where
                firstSet = S.fromList $ takeFirst rucksack
                secondSet = S.fromList $ takeSecond rucksack
    in
        show . sum $ map (itemValue . findCommon) rucksacks


part2 :: [Rucksack] -> String
part2 rucksacks = 
    let findBadge :: Rucksack -> Rucksack -> Rucksack -> Item
        findBadge (Rucksack xs) (Rucksack ys) (Rucksack zs) =
            S.elemAt 0 $ S.fromList xs `S.intersection` S.fromList ys `S.intersection` S.fromList zs

        findBadges :: [Rucksack] -> [Item]
        findBadges (x : y : z : rest) = findBadge x y z : findBadges rest
        findBadges _ = []
    in show . sum $ map itemValue (findBadges rucksacks)


solve :: String -> IO ()
solve input = do
    let rucksacks :: [Rucksack]
        rucksacks = map (Rucksack . map Item) $ lines input
    putStrLn "-- Part 1 -- " >> putStrLn (part1 rucksacks)
    putStrLn "-- Part 2 -- " >> putStrLn (part2 rucksacks)
