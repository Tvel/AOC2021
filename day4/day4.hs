import System.IO
import Data.List.Split ( splitOn )
import Data.List ( transpose )

type Card = [[(Int,Bool)]]

main = do
    print "Part1_0"
    readInputAndProcess "input_0.txt" solvePart1
    print "Part1"
    readInputAndProcess "input_1.txt" solvePart1
    print "Part2_0"
    readInputAndProcess "input_0.txt" solvePart2
    print "Part2"
    readInputAndProcess "input_1.txt" solvePart2

readInputAndProcess :: Show a => FilePath -> (([Int], [Card]) -> a) -> IO ()
readInputAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let input = readInput contents
            print (processor input))

readInput input = parseParts (lines input)

parseParts :: [String] -> ([Int], [Card])
parseParts (x:xs) = (map read (splitOn "," x), filter (/= []) (parseCards xs))

parseCards :: [String] -> [Card]
parseCards [] = []
parseCards xs = getACard xs : parseCards (drop 6 xs)

getACard :: [String] -> Card
getACard xs = getCard (map getAcardRow (take 5 (drop 1 xs)))

getAcardRow :: String -> [Int]
getAcardRow x = map read (words x)

getCard :: [[Int]] -> Card
getCard = map (\r -> zip r (repeat False))

solvePart1 :: ([Int], [Card]) -> Int
solvePart1 (x:xs, cs) | any hasBingo markedCards = getBingoValue x (head $ filter hasBingo markedCards)
                      | otherwise = solvePart1 (xs, markedCards)
                      where
                         markedCards = map (markNumber x) cs

hasBingo :: Card -> Bool
hasBingo c | any (all snd) c = True
           | any (all snd) (transpose c) = True
           | otherwise = False

markNumber :: Int -> Card -> Card
markNumber n = map (map markTrueIfbingo)
    where markTrueIfbingo (x,v) = if x == n then (x,True) else (x,v)

getBingoValue :: Int -> Card -> Int
getBingoValue winnerValue card = winnerValue * sum (map fst (filter (\(_,x) -> not x) (concat card)))

solvePart2 :: ([Int], [Card]) -> Int
solvePart2 (x:xs, cs) | length possibleWinners == 1 = solvePart1 (xs, possibleWinners)
                      | otherwise = solvePart2 (xs, markedCards)
                      where
                         possibleWinners = filter (not . hasBingo) markedCards
                         markedCards = map (markNumber x) cs