module Main (main) where
import Data.Functor ((<$>))
import Data.List (find, singleton)
import qualified Data.Map as Map
import Data.Maybe
import System.IO
   
main = do  
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents

analyze :: [String] -> String
analyze s = show (count1 s 0)

count1 :: [String] -> Int -> Int
count1 [] acc            = acc
count1 (x : xs) acc      = count1 xs (acc + acc')
                                        where   l = length x - 1
                                                m = Map.fromListWith (++) (zip x (map singleton [0 .. l]))
                                                k = reverse (Map.keys m)
                                                acc' | length k == 1 = read ([x !! 0] ++ [x !! 0]) :: Int
                                                     | otherwise = max cand1 cand2
                                                                        where x1 = last (m Map.! (k !! 0))
                                                                              x2 = last (m Map.! (k !! 1))
                                                                              cand1 = if (x1 < x2) then read ([k !! 0] ++ [k !! 1]) :: Int else read ([k !! 1] ++ [k !! 0]) :: Int
                                                                              x3 = find (\t -> any (> x1) (m Map.! t)) k
                                                                              cand2 = if (x3 /= Nothing) then read ([k !! 0] ++ [fromJust x3]) :: Int else 0
