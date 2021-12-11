module Day8 ( main ) where

import System.IO
import Data.List.Split ( splitOn )
import Data.List ( group, sort, find, findIndex )
import Data.Set ( fromList )
import qualified Data.Set as S
import qualified Data.Map as M

main = do
    print "----DAY 8----"
    print "Part1_0"
    readInputAndProcess "inputs/day8/input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "inputs/day8/input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "inputs/day8/input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "inputs/day8/input_1.txt" solvePart2

readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> [([S.Set Char], [S.Set Char])]
readInput input = ((\xs -> (head xs, xs!!1)) . tosets <$> (words <$>)) . splitOn " | " <$> lines input
    where tosets [s,n] = [S.fromList <$> s, S.fromList <$> n]

solvePart1 :: [([S.Set Char], [S.Set Char])] -> Int
solvePart1 = calc1478 0

calc1478 :: Int -> [([S.Set Char], [S.Set Char])] -> Int
calc1478 c [] = c
calc1478 c ((patterns, numbers):xs) = calc1478 (c + calc) xs
    where calc = length $ filter (== True) $ (==) <$> numbers <*> M.elems (determineKnownNumbers patterns)

determineKnownNumbers :: [S.Set Char] -> M.Map Int (S.Set Char)
determineKnownNumbers patterns = populateKnown patterns M.empty

populateKnown :: [S.Set Char] -> M.Map Int (S.Set Char) -> M.Map Int (S.Set Char)
populateKnown patterns map = addToMap 8 get8 $ addToMap 7 get7 $ addToMap 4 get4 $ addToMap 1 get1 map
    where get1 = head $ getBylen 2
          get8 = head $ getBylen 7
          get7 = head $ getBylen 3
          get4 = head $ getBylen 4
          addToMap n oneSet map = M.insertWith S.union n oneSet map
          getBylen l = filter (\x -> length x == l) patterns

solvePart2 i = sum (calc <$> i)
    where calc (patterns, numbers) = foldl1 (\x y -> 10*x+y) $ (getMap M.!) <$> numbers
            where getMap = reverseMap $ determineNumbers patterns

determineNumbers :: [S.Set Char] -> M.Map Int (S.Set Char)
determineNumbers patterns = polulateOthers patterns $ populateKnown patterns M.empty

polulateOthers :: [S.Set Char] -> M.Map Int (S.Set Char) -> M.Map Int (S.Set Char)
polulateOthers patterns map =  with0
    where with3 = M.insert 3 (find3 patterns map) map
          with5 = M.insert 5 (find5 patterns with3) with3
          with2 = M.insert 2 (find2 patterns with5) with5
          with9 = M.insert 9 (find9 patterns with2) with2
          with6 = M.insert 6 (find6 patterns with9) with9
          with0 = M.insert 0 (find0 patterns with6) with6

find3,find5,find2,find9,find6,find0 :: [S.Set Char] -> M.Map Int (S.Set Char) -> S.Set Char
find3 patterns map = S.union (map M.! 7) $ head $ filter (\x -> length x == 2) $ flip S.difference (map M.! 7) <$> filter (\x -> length x == 5) patterns

find5 patterns map = get possible $ findIndex (\x -> length x == 2) $ flip S.difference (map M.! 4) <$> possible
    where possible = (filter (\x -> x /= (map M.! 3)) . filter (\x -> length x == 5) ) patterns

find2 patterns map = head $ (filter (\x -> x /= (map M.! 5)) . filter (\x -> x /= (map M.! 3)) . filter (\x -> length x == 5)) patterns 

find9 patterns map = get possible $ findIndex null $ flip S.difference (S.union (map M.! 3) (map M.! 5)) <$> possible
    where possible = filter (\x -> length x == 6) patterns 

find6 patterns map = get possible $ findIndex (\x -> length x == 1) $ S.difference (map M.! 1) <$> possible
    where possible = (filter (\x -> x /= (map M.! 9)) . filter (\x -> length x == 6)) patterns 

find0 patterns map = head $ filter (\x -> x /= (map M.! 9)) $ filter (\x -> x /= (map M.! 6)) $ filter (\x -> length x == 6) patterns 

get possible (Just val) = possible !! val
get possible Nothing = error "oops"

reverseMap map = M.fromList $ (\(x,y) -> (y,x)) <$> M.toList map