module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split
import Data.List (notElem)
import System.IO

type Graph a = [(a, [a])]
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze1 (lines contents)

analyze1 :: [String] -> String
analyze1 s = show $ helper d ingr 0
                where d = concatMap (\x -> let [a, b] = splitOn ("-") x in [(read a :: Int, read b :: Int)] ) $ takeWhile (/= "") s
                      ingr = tail $ dropWhile (/= "") s


helper d [] acc        = acc
helper d (x  : xs) acc = helper d xs acc'
                                where   x' = read x :: Int
                                        acc' | any (\(a, b) -> if ((x' >= a) && (x' <= b)) then True else False) d = acc + 1
                                             | otherwise = acc
