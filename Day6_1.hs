module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitWhen)
import Data.List (transpose, init)
import Data.Char (isDigit)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze (lines contents)

analyze :: [String] -> String
analyze s = show $ helper numbs m 0
                        where numbs = transpose $ map (\str -> map (\x -> read x :: Int) $ filter (/= "") $ splitWhen (not . isDigit) str) (init s)
                              m = filter (/= ' ' ) (last s)
                              
helper [] _ acc = acc
helper (x : xs) (m : ms) acc = helper xs ms (acc + acc')
                                        where acc' | m == '*' = foldl (*) 1 x
                                                   | otherwise = foldl (+) 0 x
