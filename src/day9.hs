module Day9 ( main ) where

import System.IO
import Data.Matrix ( Matrix, fromLists, mapPos, safeGet, toList, getElem )
import Data.Maybe  ( catMaybes )
import Data.Char (digitToInt)
import qualified Data.Set as S
import Data.List (nub, sort)

type Point = (Int,Int)

main = do
    print "----DAY 9----"
    print "Part1_0"
    readInputAndProcess "inputs/day9/input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day9/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day9/input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day9/input_1.txt" solvePart2

readInputAndProcess :: Show a => FilePath -> (Matrix Int -> a) -> IO ()
readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> Matrix Int
readInput = fromLists . fmap (digitToInt <$>) . lines

solvePart1 input = sum $ (+1) . fst <$> filter snd (toList mapLowestPoints)
    where mapLowestPoints = mapPos (\(r,c) a -> (a, a < (minimum $ snd <$> neighbours input (r,c)) )) input

neighbours :: Matrix Int -> Point -> [(Point , Int)]
neighbours m (x, y) = [(p, n) | (p, Just n) <- ns]
    where points = [(x+dx, y+dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
          ns = (\(x, y) -> ((x, y), safeGet x y m)) <$> points

solvePart2 input = product $ take 3 $ reverse $ sort $ length <$> nub <$> findBasins input [] <$> pure <$> (\(r,c) -> ((r,c),(getElem r c input))) <$> fst <$> mapLowestPoints
    where mapLowestPoints = filter snd ( toList ( mapPos (\(r,c) a -> ((r,c), a < (minimum $ snd <$> neighbours input (r,c)) )) input))

findBasins :: Matrix Int -> [(Point, Int)] -> [(Point, Int)] -> [(Point, Int)]
findBasins m agg [] = agg
findBasins m agg (st@(startPoint,startValue):xs) = findBasins m (st:agg) $ getValidNeighbours m st ++ xs

getValidNeighbours :: Matrix Int -> (Point, Int) -> [(Point, Int)]
getValidNeighbours m st@(startPoint,startValue) = filter (\(p,i) -> valid startValue p i) $ neighbours m startPoint

valid :: Int -> Point -> Int -> Bool
valid current point int
    | int == 9 = False
    | current > int = False
    | current < int = True
    | otherwise = False