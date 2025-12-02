module Main (main) where
import Data.Functor ((<$>))
import Data.List (nub)
import Data.List.Split (splitOn, chunksOf)
import System.IO
   
main = do  
    contents <- splitOn "," <$> readFile "input.txt"
    putStrLn $ analyze contents
    
    
isInvalidID1 :: String -> Int
isInvalidID1 x = let l = length x `div` 2 in if take l x == drop l x then (read x) :: Int else 0

countInvalidIDs1 :: (Int, Int) -> Int -> Int
countInvalidIDs1 (a, b) acc | (a == b) = acc + isInvalidID1 (show b)
                            | otherwise = countInvalidIDs1 (a + 1, b) (acc + isInvalidID1 (show a))    

isInvalidID2 :: String -> Int
isInvalidID2 x = let l = length x `div` 2 in if helper 1 l x == True then (read x) :: Int else 0

helper :: Int -> Int -> String -> Bool
helper cur l x   | l == 0 = False
                 | l == 1 = if (length (nub x) == 1) then True else False
                 | cur == 1 = if length (nub x) == 1 then True else helper (cur + 1) l x
                 | cur == l = if length (nub (chunksOf cur x)) == 1 then True else False
                 | otherwise = if length (nub (chunksOf cur x)) == 1 then True else helper (cur + 1) l x

countInvalidIDs2 :: (Int, Int) -> Int -> Int
countInvalidIDs2 (a, b) acc | (a == b) = acc + isInvalidID2 (show b)
                            | otherwise = countInvalidIDs2 (a + 1, b) (acc + isInvalidID2 (show a))


analyze :: [String] -> String
analyze s = show (count1 s 0) ++ " " ++ show (count2 s 0)

count1 :: [String] -> Int -> Int
count1 [] acc            = acc
count1 (x : xs) acc      = count1 xs (acc + countInvalidIDs1 (a, b) 0)
                                        where [a', b'] = splitOn "-" x
                                              a = (read a') :: Int
                                              b = (read b') :: Int
                                              
count2 :: [String] -> Int -> Int
count2 [] acc            = acc
count2 (x : xs) acc      = count2 xs (acc + countInvalidIDs2 (a, b) 0)
                                        where [a', b'] = splitOn "-" x
                                              a = (read a') :: Int
                                              b = (read b') :: Int                                              
