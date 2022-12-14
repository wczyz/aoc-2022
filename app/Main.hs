module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import qualified Day08 (solve)

solutions =
    Map.fromList
        [ ("01", Day01.solve)
        , ("02", Day02.solve)
        , ("03", Day03.solve)
        , ("04", Day04.solve)
        , ("05", Day05.solve)
        , ("06", Day06.solve)
        , ("07", Day07.solve)
        , ("08", Day08.solve)
        ]

main :: IO ()
main = do
    args <- getArgs
    let day = head args
    case Map.lookup day solutions of
        Just f -> readFile (concat ["./data/", day, ".txt"]) >>= f
        Nothing -> putStrLn $ concat ["Day ", day, " not implemented"]
