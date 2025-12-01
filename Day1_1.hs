module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import System.IO
   
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) (0, 50))

count1 :: [String] -> (Int, Int) -> Int
count1 [] (cnt, acc)       = cnt
count1 (x : xs) (cnt, acc)  | head x == 'R' = let ans = ((acc + x') `mod` 100) in
														if (ans == 0) then count1 xs (cnt + 1, ans) else count1 xs (cnt, ans) 
              							| otherwise = let ans = ((acc - x') `mod` 100)
              													in if (ans == 0) then count1 xs (cnt + 1, ans) else count1 xs (cnt, ans) 
              								where x' = (read (tail x) :: Int)
