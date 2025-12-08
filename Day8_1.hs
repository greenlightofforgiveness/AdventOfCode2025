module Main (main) where
import Data.Functor ((<$>))
import Data.List (sortBy, sort, find, delete, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze $ lines contents

analyze :: [String] -> String
analyze s = show $ (ans !! 0) * (ans !! 1) * (ans !! 2)
                where s' = map (\t -> let [x, y, z] = splitOn "," t in (read x :: Double, read y :: Double, read z :: Double)) s
                      pairs = [ (a, b) | a <- s', b <- s', a > b]
                      dist = map (\((x1, y1, z1), (x2, y2, z2)) -> let d = sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2) in ((x1, y1, z1), (x2, y2, z2), d)) pairs
                      ans = helper (sortBy (\(_, _, d1) (_, _, d2) -> compare d1 d2) dist) [] 0
                      
helper :: [((Double, Double, Double), (Double, Double, Double), Double)] -> [Set.Set (Double, Double, Double)] -> Int -> [Int]
helper dist cs 1000 = reverse $ sort $ map Set.size cs
helper ((p1, p2, d) : ds) cs i = helper ds cs' (i + 1)
                                        where cs1 = find (Set.member p1) cs
                                              cs2 = find (Set.member p2) cs
                                              cs' | (cs1 /= Nothing) && (cs2 /= Nothing) && (cs1 == cs2) = cs
                                                  | (cs1 /= Nothing) && (cs2 /= Nothing) = (delete (fromJust cs2) (delete (fromJust cs1) cs)) ++ [Set.union (fromJust cs1) (fromJust cs2)]
                                                  | cs1 /= Nothing = (delete (fromJust cs1) cs) ++ [Set.insert p2 (fromJust cs1)]
                                                  | cs2 /= Nothing = (delete (fromJust cs2) cs) ++ [Set.insert p1 (fromJust cs2)]
                                                  | otherwise = cs ++ [Set.fromList [p1, p2]]
