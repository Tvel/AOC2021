module Day12 ( main ) where

import System.IO
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

main = do
    print "----DAY 12----"
    print "Part1_0"
    readInputAndProcess "inputs/day12/input_0.txt" solvePart1
    print "Part1_2"
    readInputAndProcess "inputs/day12/input_2.txt" solvePart1
    print "Part1_3"
    readInputAndProcess "inputs/day12/input_3.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day12/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day12/input_0.txt" solvePart2
    print "Part2_2"
    readInputAndProcess "inputs/day12/input_2.txt" solvePart2
    print "Part2_3"
    readInputAndProcess "inputs/day12/input_3.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day12/input_1.txt" solvePart2

readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

solvePart1 input = walk caves (S.filter small (M.keysSet caves), False) "start"
    where caves = foldr (\(a, b) -> M.insertWith (++) b [a] . M.insertWith (++) a [b]) M.empty input

solvePart2 input = walk caves (S.filter small (M.keysSet caves), True) "start"
    where caves = foldr (\(a, b) -> M.insertWith (++) b [a] . M.insertWith (++) a [b]) M.empty input

readInput :: String -> [(String, String)]
readInput = map parse . lines

parse :: String -> (String, String)
parse x = (take i x, drop (succ i) x)
    where i = fromJust $ elemIndex '-' x

walk :: M.Map String [String] -> (S.Set String, Bool) -> String -> Int
walk caves visit cave = if cave == "end" then 1 else
  maybe 0 (sum . flip map (caves M.! cave) . walk caves) $ step visit cave

step :: (S.Set String, Bool) -> String -> Maybe (S.Set String, Bool)
step (visit, twice) cave
  | small cave = if S.notMember cave visit then
      if twice && cave /= "start" then Just (visit, False) else Nothing
      else Just (S.delete cave visit, twice)
  | otherwise = Just (visit, twice)

small :: String -> Bool
small = (> 'Z') . head