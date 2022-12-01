module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

solutions = Map.fromList []

main :: IO ()
main = do
    args <- getArgs
    let day = head args
    case Map.lookup day solutions of
        Just f -> readFile (concat ["./data/", day, ".txt"]) >>= f
        Nothing -> putStrLn $ concat ["Day ", day, " not implemented"]
