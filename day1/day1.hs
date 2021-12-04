import System.IO

main = do
    print "Part1_0"
    readIntsAndProcess "input_0.txt" (snd . solvePart1)
    print "Part1"
    readIntsAndProcess "input_1.txt" (snd . solvePart1)
    print "Part2_0"
    readIntsAndProcess "input_0.txt" (snd . solvePart2)
    print "Part2"
    readIntsAndProcess "input_1.txt" (snd . solvePart2)


readInts :: String -> [Int]
readInts c = read <$> lines c

readIntsAndProcess filename processor = do
    withFile filename ReadMode (\handle -> do
            contents <- hGetContents handle
            let ints = readInts contents
            print (processor ints))


solvePart1 :: [Int] -> (Int,Int)
solvePart1 [] = (0,0)
solvePart1 [x] = (0,0)
solvePart1 (x:xs) = foldl helperfolder (x, 0) xs
    where
        helperfolder (last, summ) number = if number > last then (number,  summ + 1) else (number, summ)

solvePart2 :: [Int] -> ((Int, Int, Int),Int)
solvePart2 [] = ((0,0,0),0)
solvePart2 [x] = ((x,0,0),0)
solvePart2 [x, y] = ((x,y,0),0)
solvePart2 [x, y, z] = ((x,y,z),0)
solvePart2 (x:y:z:xs) = foldl helperfolder ((x, y, z), 0) xs
        where
            helperfolder ((l1, l2, l3), summ) number =
                if (number + l2 + l3) > (l1 + l2 + l3)
                    then ((l2, l3, number),  summ + 1)
                    else ((l2, l3, number), summ)

