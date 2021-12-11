module Day2 ( main ) where

import System.IO

main = do
    print "----DAY 2----"
    print "Part1_0"
    readInputAndProcess "inputs/day2/input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day2/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day2/input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day2/input_1.txt" solvePart2

data Direction = Forward | Down | Up deriving (Show)  

readInput :: String -> [(Direction, Int)]
readInput c = parse . words <$> lines  c

parse :: [[Char]] -> (Direction, Int)
parse [] = error ""
parse [x] = error ""
parse ["forward",x] = (Forward, read x)
parse ["down",x] = (Down, read x)
parse ["up",x] = (Up, read x)
parse (x:xs) = error ""

readInputAndProcess :: Show a => FilePath -> ([(Direction, Int)] -> a) -> IO ()
readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

solvePart1 input = multi $ foldl folder (0,0) input
    where folder (horisontal, depth) (Up, ammount) = (horisontal, depth - ammount)
          folder (horisontal, depth) (Down, ammount) = (horisontal, depth + ammount)
          folder (horisontal, depth) (Forward, ammount) = (horisontal + ammount, depth)
          multi (x,y) = x * y

solvePart2 input = multi $ foldl folder (0,0,0) input
    where folder (horisontal, depth, aim) (Up, ammount) = (horisontal, depth, aim - ammount)
          folder (horisontal, depth, aim) (Down, ammount) = (horisontal, depth, aim + ammount)
          folder (horisontal, depth, aim) (Forward, ammount) = (horisontal + ammount, depth + (aim * ammount), aim)
          multi (x,y,z) = x * y