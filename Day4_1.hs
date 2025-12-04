module Main (main) where
import Data.Functor ((<$>))
import System.IO

type Graph a = [(a, [a])]
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze1 (lines contents)

analyze1 :: [String] -> String
analyze1 s = show $ length $ filter (\x -> length (snd x) < 4) (buildGraph s (0, 0) (row_max, col_max) [])
                where (row_max, col_max) = ((length s) - 1, (length (s !! 0)) - 1)


buildGraph :: [String] -> (Int, Int) -> (Int, Int) -> Graph (Int, Int) -> Graph (Int, Int)
buildGraph s (row, col) (row_max, col_max) acc   | (row == row_max) && (col == col_max) = (acc ++ acc')
                                                 | col == col_max = buildGraph s (row + 1, 0) (row_max, col_max) (acc ++ acc')
                                                 | otherwise = buildGraph s (row, col + 1) (row_max, col_max) (acc ++ acc')
                                                        where dRow   = [(-1), 0, 1,  0,   1, (-1),   1, (-1)]
                                                              dCol   = [  0,  1, 0, (-1), 1, (-1), (-1), 1  ]
                                                              acc' = if (((s !! row) !! col) /= '@') then [] else [((row, col), concatMap func [0 .. 7])]
                                                              func dir = let nRow = row + dRow !! dir
                                                                             nCol = col + dCol !! dir
                                                                                                in if nRow > row_max || nRow < 0 || nCol > col_max || nCol < 0
                                                                                                   then []
                                                                                                   else if (((s !! nRow) !! nCol) == '@')
                                                                                                   then [(nRow, nCol)]
                                                                                                   else []
