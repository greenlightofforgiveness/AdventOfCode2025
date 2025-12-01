module Main (main) where
import Data.Functor ((<$>))
import System.IO
   
main = do  
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents

analyze :: [String] -> String
analyze s = show (count1 s (0, 50)) ++ " " ++ show (count2 s (0, 50))

count1 :: [String] -> (Int, Int) -> Int
count1 [] (cnt, acc)       = cnt
count1 (x : xs) (cnt, acc)  | head x == 'R' = let ans = ((acc + x') `mod` 100) in
														if (ans == 0) then count1 xs (cnt + 1, ans) else count1 xs (cnt, ans) 
							| otherwise = let ans = ((acc - x') `mod` 100)
													in if (ans == 0) then count1 xs (cnt + 1, ans) else count1 xs (cnt, ans) 
								where x' = (read (tail x) :: Int)

-- Formula for Left case: https://github.com/Tarmean/AOC2025/blob/main/src/Day1.hs
count2 :: [String] -> (Int, Int) -> Int
count2 [] (cnt, acc)       = cnt
count2 (x : xs) (cnt, acc)  | head x == 'R' = let ans = (acc + x') `mod` 100
                                                  cnt' = (acc + x') `div` 100
													in count2 xs (cnt + cnt', ans)
							| otherwise     = let ans = (acc - x') `mod` 100
							                      cnt' = ((100 - acc) `mod` 100 + x') `div` 100							
													in count2 xs (cnt + cnt', ans)
								where x' = (read (tail x) :: Int)
