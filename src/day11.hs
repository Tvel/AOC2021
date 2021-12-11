module Day11 ( main ) where

import System.IO
import Data.Matrix ( Matrix, fromLists, mapPos, safeGet, toList, getElem, setElem )
import Data.Char (digitToInt)

data Status = None | Flashed | Done deriving (Show, Eq)

main = do
    print "----DAY 11----"
    print "Part1_0"
    readInputAndProcess "inputs/day11/input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day11/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day11/input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day11/input_1.txt" solvePart2

readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> Matrix Int
readInput = fromLists . fmap (digitToInt <$>) . lines

solvePart1 i = fst $ part1step [1..100] (0, prepare)
    where prepare = mapPos (\_ a -> (a, None)) i

part1step [] (c, m) = (c, m)
part1step (x:xs) (c, m) = part1step xs (c + countFlashed, ff)
        where flashed = flashBiggerThan9 $ incrementAllBy1 $ mapPos (\_ (n,_) -> (n, None)) m
              ff = mapPos (\_ (n,_) -> (n, None)) $ flashN (getAllFlashed flashed) flashed
              countFlashed = length $ filter (\(n,s) -> n == 0) $ toList ff

flashN :: [(Int, Int)] -> Matrix (Int, Status) -> Matrix (Int, Status)
flashN [] m = m
flashN (x:xs) m = flashN (getAllFlashed (flashBiggerThan9 incAll)) incAll
    where s = setElem (0, Done) x m
          increment ((r,c):xs) m = increment xs $ setElem (inc (getElem r c m)) (r,c) m
          increment [] m = m
          incAll = increment (neighboursThatHaveNotFlashed s x) s

incrementAllBy1 = mapPos mapper
    where mapper _ (n, None) = (n+1, None)
          mapper _ (n, f) = (n, f)

flashBiggerThan9 :: (Ord a, Num a) => Matrix (a, Status) -> Matrix (a, Status)
flashBiggerThan9 = mapPos (\_ (n,f) -> if n > 9 then (0, Flashed) else (n,f) )

getAllFlashed :: Matrix (Int, Status) -> [(Int, Int)]
getAllFlashed m = (\(r,c,s) -> (r,c)) <$> ( filter (\(r,c,s) -> s == Flashed) $ toList $ mapPos (\(r,c) (n,f) -> (r,c,f)) m)

neighboursThatHaveNotFlashed :: Matrix (Int, Status) -> (Int, Int) -> [(Int, Int)]
neighboursThatHaveNotFlashed m (x, y) = [p | (p, Just (n,f)) <- ns, f == None ]
    where points = [(x+dx, y+dy) | (dx, dy) <- [(-1, -1), (-1, 0), (-1, 1), (1, 1), (1, 0), (1, -1), (0, -1), (0, 1)]]
          ns = (\(x, y) -> ((x, y), safeGet x y m)) <$> points

inc :: (Int, Status) -> (Int, Status)
inc (n, None) = (n+1, None)
inc (n, f) = (n, f)

solvePart2 i = fst $ part2step 0 (0, prepare)
    where prepare = mapPos (\_ a -> (a, None)) i

part2step cc (100, m) = (cc, m)
part2step cc (c, m) = part2step (cc+1) (countFlashed, ff)
        where flashed = flashBiggerThan9 $ incrementAllBy1 $ mapPos (\_ (n,_) -> (n, None)) m
              ff = mapPos (\_ (n,_) -> (n, None)) $ flashN (getAllFlashed flashed) flashed
              countFlashed = length $ filter (\(n,s) -> n == 0) $ toList ff
