import System.IO
import Text.Parsec
import qualified Data.IntMap as M
import Data.IntMap ((!?))
import Data.Either ( fromRight )
import qualified Data.Maybe

main = do
    print "Part1_0"
    readInputAndProcess "input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "input_1.txt" solvePart2

readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> [Int]
readInput input = fromRight [] $ parse (sepBy number (char ',')) "" input
    where number = read <$> many1 digit

solvePart1 input = run (setupMap input) 80

solvePart2 input = run (setupMap input) 256

setupMap :: (Foldable t, Num a) => t M.Key -> M.IntMap a
setupMap = foldr (\x map -> M.insertWith (+) x 1 map) M.empty

run map 0 = M.foldr (+) 0 map
run map day = run mapReset (day-1)
  where mapDecremented = M.mapKeys pred map
        numKids = Data.Maybe.fromMaybe 0 (map !? 0)
        mapInsertNew = M.insert 8 numKids mapDecremented
        mapReset = M.insertWith (+) 6 numKids (M.delete (-1) mapInsertNew)