module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze $ lines contents

analyze :: [String] -> String
analyze s = show $ maximum areas
                where s' = map (\t -> let [c, r] = splitOn "," t in (read r :: Int, read c :: Int)) s
                      pairs = [ ((r1, c1), (r2, c2)) | (r1, c1) <- s', (r2, c2) <- s', c1 <= c2]
                      areas = map (\((r1, c1), (r2, c2)) -> (abs (r2 - r1 + 1)) * (abs (c2 - c1 + 1))) pairs
