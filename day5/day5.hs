import System.IO
import Data.List.Split ( splitOn )
import Data.List ( group, sort )

type Coord = (Integer, Integer)
type Vent = (Coord, Coord)

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

readInput input = (toCoord <$> (splitOn "," <$>)) . splitOn " -> " <$> lines input

toCoord :: [[String]] -> Vent
toCoord [[x1,y1],[x2,y2]] = ((read x1, read y1), (read x2, read y2))

solvePart1 i = length $ filter (\x -> length x > 1) $ group $ sort $ concat $ getCoordinatesStraightOnly <$> i

getCoordinatesStraightOnly :: Vent -> [Coord]
getCoordinatesStraightOnly ((x1,y1),(x2, y2)) | x1 == x2 = [(x1,y)| y <- [y1..y2]++[y2..y1]]
                                              | y1 == y2 = [(x,y1)| x <- [x1..x2]++[x2..x1]]
                                              | otherwise = []

solvePart2 i = length $ filter (\x -> length x > 1) $ group $ sort $ concat $ getCoordinates <$> i

getCoordinates :: Vent -> [Coord]
getCoordinates ((x1,y1),(x2, y2)) | x1 == x2 = [(x1,y)| y <- [y1..y2]++[y2..y1]]
                                  | y1 == y2 = [(x,y1)| x <- [x1..x2]++[x2..x1]]
                                  | otherwise = getDiag start end
                                  where start = if x1 < x2 then (x1,y1) else (x2,y2)
                                        end = if x1 < x2 then (x2,y2) else (x1,y1)

getDiag :: Coord -> Coord -> [Coord]
getDiag start@(x1,y1) (x2,y2) = getDiagCoords start len getYDir
    where len = abs (x1-x2)
          getYDir = if y1 < y2 then 1 else -1
          getDiagCoords c@(x,y) 0 _ = [c]
          getDiagCoords c@(x,y) len ydir = c : getDiagCoords (x+1,y+ydir) (len-1) ydir
