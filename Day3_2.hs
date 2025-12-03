module Main (main) where
import Data.Functor ((<$>))
import Data.List (find, singleton)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.IO
   
main = do  
    contents <- lines <$> readFile "input.txt"
    putStrLn $ analyze contents

analyze :: [String] -> String
analyze s = show (count2 s 0)

count2 :: [String] -> Int -> Int
count2 [] acc            = acc
count2 (x : xs) acc      = count2 xs (acc + acc')
                                        where   l = length x - 1
                                                m' = Map.fromListWith (++) (zip x (map singleton [0 .. l]))
                                                m = Map.map reverse m'
                                                k = reverse (Map.keys m)
                                                acc' | length k == 1 = read (take 12 (repeat (x !! 0))) :: Int  
                                                     | otherwise = ans
                                                                        where maxIndex = l + 1 - 12
                                                                              x1 = find (\t -> any (<= maxIndex) (m Map.! t)) k
                                                                              minIndex = fromJust (find (<= maxIndex) (m Map.! (fromJust x1)))
                                                                              ans = helper m k l minIndex 11 (read ([fromJust x1]) :: Int)
helper :: Map.Map Char [Int] -> String -> Int -> Int -> Int -> Int -> Int
helper m k l minIndex 0 ans = ans
helper m k l minIndex cnt ans = helper m k l minIndex' (cnt - 1) ans'
                                        where ans' = ans * 10 + x
                                              maxIndex = l + 1 - cnt
                                              x' = find (\t -> (any (\x -> (x > minIndex) && (x <= maxIndex)) (m Map.! t))) k
                                              x = read ([fromJust x']) :: Int
                                              minIndex' = fromJust (find (\x -> (x > minIndex) && (x <= maxIndex)) (m Map.! (fromJust x')))
