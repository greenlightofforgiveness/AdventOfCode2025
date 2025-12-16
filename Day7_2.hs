module Main (main) where
import Data.Functor ((<$>))
import Data.List (elem)
import qualified Data.Map as Map
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze (lines contents)

analyze :: [String] -> String
analyze s = show $ sum $ map snd $ Map.toList $ helper 0 posSps (row_max, col_max) acc
                where (row_max, col_max) = ((length s) - 1, (length (s !! 0)) - 1)
                      posSps = findSps s (0,0) (row_max, col_max) []
                      posS = findS s (0,0) (row_max, col_max)
                      acc = Map.insert (snd posS) 1 (Map.fromList (zip [0 .. col_max](repeat 0)))

findS :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
findS s (row, col) (row_max, col_max) | (s !! row) !! col == 'S' = (row, col)
                                      | col == col_max = findS s (row + 1, 0) (row_max, col_max)
                                      | otherwise = findS s (row, col + 1) (row_max, col_max)
                                      
findSps :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findSps s (row, col) (row_max, col_max) acc | ((col == col_max) && (row == row_max)) = acc ++ acc'
                                            | col == col_max = findSps s (row + 1, 0) (row_max, col_max) (acc ++ acc')
                                            | otherwise = findSps s (row, col + 1) (row_max, col_max) (acc ++ acc')
                                                    where acc' = if (s !! row) !! col == '^' then [(row, col)] else []
                                                    
helper :: Int -> [(Int, Int)] -> (Int, Int) -> Map.Map Int Int -> Map.Map Int Int
helper row posSps (row_max, col_max) acc | row == row_max = acc
                                         | otherwise = helper (row + 1) posSps (row_max, col_max) acc' 
                                                where acc' = Map.fromListWith (+) $ concatMap (\(c, x) -> func c x row col_max posSps) (Map.toList acc)
                                                
func c x row col_max posSps | ((row + 1, c) `elem` posSps) && (c > 0) && (c < col_max) = [(c - 1, x), (c + 1, x), (c, 0)]
                            | ((row + 1, c) `elem` posSps) && (c > 0) = [(c - 1, x), (c, 0)]
                            | ((row + 1, c) `elem` posSps) && (c < col_max) = [(c + 1, x), (c, 0)]
                            | otherwise = [(c, x)]
