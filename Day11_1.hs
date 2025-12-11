module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import System.IO

-- https://wiki.haskell.org/99_questions/Solutions/81

import Data.List (partition)

pathsImpl :: Eq a => [a] -> a -> a -> [(a, a)] -> [[a]]
pathsImpl trail src dest clauses
    | src == dest = [src:trail]
    | otherwise = do
        let (nexts, rest) = partition ((==src) . fst) clauses
        next <- nexts
        pathsImpl (src:trail) (snd next) dest rest

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths src dest clauses = map reverse (pathsImpl [] src dest clauses)
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze $ lines contents

analyze :: [String] -> String
analyze s = show $ length $ paths "you" "out" g
                        where g = concatMap (\x -> let [a, b] = splitOn ":" x in zip (repeat a) (splitOn " " b)) s
