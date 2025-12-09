module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze $ lines contents

analyze :: [String] -> String
analyze s = show $ maximum areas
                where s' = map (\t -> let [x, y] = splitOn "," t in (read x :: Int, read y :: Int)) s
                      pairs = [ ((x1, y1), (x2, y2)) | (x1, y1) <- s', (x2, y2) <- s', x1 <= x2, y1 <= y2]
                      areas = map (\((x1, y1), (x2, y2)) -> (x2 - x1 + 1) * (y2 - y1 + 1)) pairs
