module Main (main) where
import Data.Functor ((<$>))
import Data.List
import Data.List.Split (splitOn)
import System.IO
   
main = do  
    contents <- splitOn "," <$> readFile "input.txt"
    putStrLn $ analyze contents

isInvalidID :: String -> Int
isInvalidID x = let l = length x `div` 2 in if take l x == drop l x then (read x) :: Int else 0

countInvalidIDs :: (Int, Int) -> Int -> Int
countInvalidIDs (a, b) acc | (a == b) = acc + isInvalidID (show b)
                           | otherwise = countInvalidIDs (a + 1, b) (acc + isInvalidID (show a))


analyze :: [String] -> String
analyze s = show (count1 s 0) -- ++ " " ++ show (count2 s (0, 50))

count1 :: [String] -> Int -> Int
count1 [] acc            = acc
count1 (x : xs) acc      = count1 xs (acc + countInvalidIDs (a, b) 0)
                                        where [a', b'] = splitOn "-" x
                                              a = (read a') :: Int
                                              b = (read b') :: Int
