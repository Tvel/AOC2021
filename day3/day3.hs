import System.IO
import Text.Html ( input )
import Control.Applicative ()
import Data.List ( transpose )
import Numeric ( readInt )
import Data.Char ( digitToInt )

main = do
    print "Part1_0"
    readInputAndProcess "input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "input_1.txt" solvePart2

readInputAndProcess :: Show a => FilePath -> ([String] -> a) -> IO ()
readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput :: String -> [String]
readInput = lines

solvePart1 input = multi . toNum . foldl makeGamaEpsilonStrings ("", "") $ map (makeGamaEpsilonDigits . countBits) (transpose input)
    where makeGamaEpsilonStrings (g, e) (gd, ed) = (g ++ gd, e ++ ed)
          countBits str = ((length . filter (== '0')) str, (length . filter (== '1')) str)
          makeGamaEpsilonDigits (zeros,ones) = if zeros > ones then ("0","1") else ("1","0")

solvePart2 input = multi $ toNum (readO2 , readCO2)
    where readO2 = head (solvePart2Readings input 0 (countBitsAll input) filterOxy)
          readCO2 = head (solvePart2Readings input 0 (countBitsAll input) filterCO2)
          filterOxy (zeros,ones) = if ones >= zeros then '1' else '0'
          filterCO2 (zeros,ones) = if zeros <= ones then '0' else '1'

solvePart2Readings :: [String] -> Int -> [(Int, Int)] -> ((Int, Int) -> Char) -> [String]
solvePart2Readings [x] pos mapping f = [x]
solvePart2Readings input pos mapping f = 
    let filtered = filter ff input in 
         solvePart2Readings filtered (pos + 1) (countBitsAll filtered) f
    where
          ff str = str!!pos == f (mapping!!pos)

countBitsAll input = map countBits (transpose input)
    where
        countBits str = ((length . filter (== '0')) str, (length . filter (== '1')) str)

readBin = head . fmap fst . readInt 2 (`elem` "01") digitToInt

toNum (g,e) = (readBin g, readBin e)

multi (x,y) = x * y