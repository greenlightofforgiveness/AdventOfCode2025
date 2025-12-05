module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import Data.List (sort)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze2 (lines contents)

analyze2 :: [String] -> String
analyze2 s = show $ helper (sort d) (-1) 0
                where d = concatMap (\x -> let [a, b] = splitOn ("-") x in [(read a :: Int, read b :: Int)] ) $ takeWhile (/= "") s
                      
-- Idea: https://aoc-puzzle-solver.streamlit.app/

helper :: [(Int, Int)] -> Int -> Int -> Int
helper [] prevUpper cnt              = cnt
helper ((a, b) : xs) prevUpper cnt  | b <= prevUpper = helper xs prevUpper cnt
                                    | otherwise = helper xs b cnt'
                                                where cnt' = cnt + b - (max a (prevUpper + 1)) + 1
