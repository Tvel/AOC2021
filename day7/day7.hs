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

solvePart1 :: [Int] -> Int
solvePart1 = solve id

solvePart2 :: [Int] -> Int
solvePart2 = solve increasingFuel

increasingFuel :: Int -> Int
increasingFuel n = (1 + n) * n `div` 2

cost :: (Int -> Int) -> Int -> [Int] -> Int
cost calcFuel n = sum . fmap (calcFuel . abs . subtract n)

solve :: (Int -> Int) -> [Int] -> Int
solve calcFuel xs = minimum $ fmap mapper [minimum xs..maximum xs]
    where mapper x = cost calcFuel x xs