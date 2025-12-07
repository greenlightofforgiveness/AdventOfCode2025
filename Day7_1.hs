module Main (main) where
import Data.Functor ((<$>))
import Data.List (elem, nub, any)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze (lines contents)

analyze :: [String] -> String
analyze s = show $ helper [posS] posSps (row_max, col_max) 0
                where (row_max, col_max) = ((length s) - 1, (length (s !! 0)) - 1)
                      posSps = findSps s (0,0) (row_max, col_max) []
                      posS = findS s (0,0) (row_max, col_max)

findS :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findS s (row, col) (row_max, col_max) | (s !! row) !! col == 'S' = (row, col)
                                      | col == col_max = findS s (row + 1, 0) (row_max, col_max)
                                      | otherwise = findS s (row, col + 1) (row_max, col_max)
                                      
findSps :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findSps s (row, col) (row_max, col_max) acc | ((col == col_max) && (row == row_max)) = acc
                                            | col == col_max = findSps s (row + 1, 0) (row_max, col_max) (acc ++ acc')
                                            | otherwise = findSps s (row, col + 1) (row_max, col_max) (acc ++ acc')
                                                    where acc' = if (s !! row) !! col == '^' then [(row, col)] else []

helper :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int -> Int
helper posBms posSps (row_max, col_max) acc | (any (\(r, c) -> r == row_max) posBms) = acc
                                            | otherwise = helper posBms'' posSps (row_max, col_max) acc'
                                                where posBms' = nub $ concatMap (\(r, c) -> if (r + 1, c) `elem` posSps then [(r + 1, c - 1), (r + 1, c + 1)] else [(r + 1, c)]) posBms     
                                                      posBms'' = filter (\(r, c) -> (r >= 0) && (r <= row_max) && (c >= 0) && (c <= col_max)) posBms'
                                                      acc' = acc + length (filter (\(r, c) -> (r + 1, c) `elem` posSps) posBms)
