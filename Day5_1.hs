module Main (main) where
import Data.Functor ((<$>))
import Data.List.Split (splitOn)
import System.IO
            
main = do  
    contents <- readFile "input.txt"
    putStrLn $ analyze1 (lines contents)

analyze1 :: [String] -> String
analyze1 s = show $ length $ filter (== True) $ map (\x -> any (\(a, b) -> ((x >= a) && (x <= b))) d) ingr
                where d = concatMap (\x -> let [a, b] = splitOn ("-") x in [(read a :: Int, read b :: Int)] ) $ takeWhile (/= "") s
                      ingr = map (\x -> read x :: Int) $ tail $ dropWhile (/= "") s
