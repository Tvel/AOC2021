module Day10 ( main ) where

import System.IO
import Data.List (sort)

main = do
    print "----DAY 10----"
    print "Part1_0"
    readInputAndProcess "inputs/day10/input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day10/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day10/input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day10/input_1.txt" solvePart2

readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> [String]
readInput = lines

score1 :: [Char] -> Int
score1 ")" = 3
score1 "]" = 57
score1 "}" = 1197
score1 ">" = 25137
score1 _ = 0

solvePart1 input =  sum $ score1 . foldl folder [] <$> input

folder :: String -> Char -> String
folder ")" _ = ")"
folder "]" _ = "]"
folder "}" _ = "}"
folder ">" _ = ">"
folder xs '(' = '(':xs
folder xs '[' = '[':xs
folder xs '{' = '{':xs
folder xs '<' = '<':xs
folder ('(':xs) ')' = xs
folder ('[':xs) ']' = xs
folder ('{':xs) '}' = xs
folder ('<':xs) '>' = xs
folder _ x = [x]

solvePart2 :: [String] -> Int
solvePart2 input = head $ middle $ sort $ score2 <$> ( filter ((>1) . length) $ foldl folder [] <$> input)

middle :: [a] -> [a]
middle xs = take (signum ((l + 1) `mod` 2) + 1) $ drop ((l - 1) `div ` 2) xs
  where l = length xs

score2 :: [Char] -> Int
score2 = foldl f 0
  where
  f acc '(' = acc * 5 + 1
  f acc '[' = acc * 5 + 2
  f acc '{' = acc * 5 + 3
  f acc '<' = acc * 5 + 4
  f _ _ = 0