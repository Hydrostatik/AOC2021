{-# LANGUAGE TupleSections #-}

module Lib (
    dayOneSolutionIO
,   dayTwoSolutionIO
,   dayThreeSolutionIO
,   dayFourSolutionIO
,   dayFiveSolutionIO
,   daySixSolutionIO
,   daySevenSolutionIO
) where

import Data.Char
import Data.List
import qualified Data.Ord as O
import qualified Data.Map as M

compareVals :: (Num a, Ord a) => [a] -> a
compareVals [] = 0
compareVals [x] = 0
compareVals xs = sum $ zipWith (\x y -> if x > y then 1 else 0) (tail xs) (init xs)

compareInWindow :: [Integer] -> Integer
compareInWindow input
    | 3 >= len = 0
    | null input = 0
    | otherwise = compareVals list
        where
            len = length input
            rem = length input - 2
            l1 = take rem input
            l2 = drop 1 $ take (rem + 1) input
            l3 = drop 2 input
            list = zipWith3 (\x y z -> x + y + z) l1 l2 l3


dayOneSolutionIO :: IO ()
dayOneSolutionIO = do
    input <- map (\x -> read x :: Integer) . lines <$> readFile "input/DayOneInput.txt"
    print $ compareVals input
    print $ compareInWindow input

data Movement
    =   Forward Integer
    |   Up Integer
    |   Down Integer

parseToMovement :: String -> Movement
parseToMovement xs
    | direction == "forward" = Forward displacement
    | direction == "up" = Up displacement
    | direction == "down" = Down displacement
    | otherwise = error "Invalid Displacement Value"
    where
        initialParse = words xs
        direction = head initialParse
        displacement = read (last initialParse) :: Integer

totalDisplacementProduct :: [Movement] -> Integer
totalDisplacementProduct xs = uncurry (*) $ foldr ((\(x,y) (w,z) -> (x+w,y+z)) . displace) (0,0) xs
    where
        displace (Forward x) =  (x, 0)
        displace (Up x) = (0, -x)
        displace (Down x) = (0, x)

totalDisplacementProductWithAim :: [Movement] -> Integer
totalDisplacementProductWithAim xs = (\(x,y,_) -> x * y) . foldl (\(w,v,k) (x,y,z)  -> (x+w,x*k+v,z+k)) (0,0,0) $ map displace xs
    where
        displace (Forward x) = (x, 0, 0)
        displace (Up x) = (0, 0, -x)
        displace (Down x) = (0, 0, x)

getDisplacementProductFromMovement :: [String] -> Integer
getDisplacementProductFromMovement = totalDisplacementProduct . map parseToMovement

getDisplacementProductFromMovementWithAim :: [String] -> Integer
getDisplacementProductFromMovementWithAim = totalDisplacementProductWithAim . map parseToMovement

dayTwoSolutionIO :: IO ()
dayTwoSolutionIO = do
    input <- lines <$> readFile "input/DayTwoInput.txt"
    print $ getDisplacementProductFromMovement input
    print $ getDisplacementProductFromMovementWithAim input

getMostFrequentBits :: [String] -> String
getMostFrequentBits xs = map checkCommonality $ foldr (zipWith (+) . map digitToInt) (repeat 0) xs
    where
        sampleSize = ceiling $ (fromIntegral $ length xs :: Double) / 2.0
        checkCommonality x =
            if x >= sampleSize
                then '1'
                else '0'

getLeastFrequentBits :: [String] -> String
getLeastFrequentBits xs = map checkCommonality $ foldr (zipWith (+) . map digitToInt) (repeat 0) xs
    where
        sampleSize = ceiling $ (fromIntegral $ length xs :: Double) / 2.0
        checkCommonality x =
            if x >= sampleSize
                then '0'
                else '1'

binary :: Integer -> Integer -> [Integer] -> Integer
binary _ val [] = val
binary acc val (x:xs) = binary (acc + 1) (2 ^ acc * x + val) xs

binaryGenerator :: [Integer] -> Integer
binaryGenerator = binary 0 0

toBinary :: String -> Integer
toBinary xs = binaryGenerator $ binaryFromString xs
    where
        binaryFromString = map (toInteger . digitToInt) . reverse

powerConsumptionRate :: [String] -> Integer
powerConsumptionRate xs = toBinary gamma * toBinary beta
    where
        gamma = getMostFrequentBits xs
        beta = map (\x -> if x == '1' then '0' else '1') gamma

oxygenGeneratorRating :: [String] -> Integer
oxygenGeneratorRating = toBinary . algo 0
    where
        algo _ [] = ""
        algo _ [x] = x
        algo pos xs = algo (pos + 1) (filter (\x -> x !! pos == val) xs)
            where
                val = getMostFrequentBits xs !! pos

co2ScrubberRating :: [String] -> Integer
co2ScrubberRating = toBinary . algo 0
    where
        algo _ [] = ""
        algo _ [x] = x
        algo pos xs = algo (pos + 1) (filter (\x -> x !! pos == val) xs)
            where
                val = getLeastFrequentBits xs !! pos

lifeSupportRating :: [String] -> Integer
lifeSupportRating xs = oxygenGeneratorRating xs * co2ScrubberRating xs

dayThreeSolutionIO :: IO ()
dayThreeSolutionIO = do
    input <- lines <$> readFile "input/DayThreeInput.txt"
    print $ powerConsumptionRate input
    print $ lifeSupportRating input

processBingoResult :: [Integer] -> [[Integer]] -> (Integer, Integer)
processBingoResult draw board = (,) (toInteger numOfMoves + 1) score
    where
        possibleLines = transpose board <> board
        numOfMoves = minimum $ map (\xs -> maximum $ findIndices (`elem` xs) draw) possibleLines
        actualDraw = take (numOfMoves + 1) draw
        score = last actualDraw * (sum . filter (`notElem` actualDraw) $ concat board)

-- I want to win
processBingoBoards :: [Integer] -> [[[Integer]]] -> Integer
processBingoBoards draw boards = snd . minimumBy (O.comparing fst) $ map (processBingoResult draw) boards

-- I want to lose
processBingoBoards' :: [Integer] -> [[[Integer]]] -> Integer
processBingoBoards' draw boards = snd . maximumBy (O.comparing fst) $ map (processBingoResult draw) boards

dayFourSolutionIO :: IO ()
dayFourSolutionIO = do
    input <- lines <$> readFile "input/DayFourInput.txt"
    let draw = stringToIntegers . map (\x -> if x == ',' then ' ' else x) $ head input
    let boards =  (map . map) stringToIntegers $ matrixSplitter $ ignoreHeader input
    print $ processBingoBoards draw boards
    print $ processBingoBoards' draw boards
        where
            stringToIntegers = map (\x -> read x :: Integer). words
            ignoreHeader = tail . tail
            matrixSplitter [] = []
            matrixSplitter input = fst m : matrixSplitter ((tail . snd) m)
                where
                    indexToSplit = elemIndex "" input
                    splitMatrix (Just val) matrix = splitAt val matrix
                    splitMatrix Nothing matrix = (matrix, [""])
                    m = splitMatrix indexToSplit input

lineCoords :: Bool -> (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
lineCoords eDiag start finish
    | xCordSame = map (fst start,) $ getRange (snd start) (snd finish)
    | yCordSame =  map (, snd start) $ getRange (fst start) (fst finish)
    | strictDiag && eDiag = zip (getRange (fst start) (fst finish)) (getRange (snd start) (snd finish))
    | otherwise = []
    where
        xCordSame = fst start == fst finish
        yCordSame = snd start == snd finish
        strictDiag = pi/4 == (atan2 (fromIntegral (abs (fst finish - fst start))) (fromIntegral (abs (snd finish - snd start))) :: Double)
        getRange x y = if x <= y then [x..y] else [x,(x-1)..y]

findAllCoordsNoDiag :: [((Integer, Integer), (Integer, Integer))] -> [(Integer, Integer)]
findAllCoordsNoDiag = concatMap (uncurry $ lineCoords False)

findAllCoords :: [((Integer, Integer), (Integer, Integer))] -> [(Integer, Integer)]
findAllCoords = concatMap (uncurry $ lineCoords True)

findIntersectingPoints :: Int -> [(Integer, Integer)] -> Integer
findIntersectingPoints threshold coords = sum $ map (\x -> if snd x >= threshold then 1 else 0) (getDups coords)
    where
        getDups xs = M.toList $ M.fromListWith (+) (zip xs (repeat 1))

findAllIntersectingPointsNoDiag :: [((Integer, Integer), (Integer, Integer))] -> Integer
findAllIntersectingPointsNoDiag xs = findIntersectingPoints 2 (findAllCoordsNoDiag xs)

findAllIntersectingPoints :: [((Integer, Integer), (Integer, Integer))] -> Integer
findAllIntersectingPoints xs = findIntersectingPoints 2 (findAllCoords xs)

dayFiveSolutionIO :: IO ()
dayFiveSolutionIO = do
    input <- fmap ((\x -> ((head x, x !! 1), (x !! 2, x !! 3))) . fmap (\x -> read x :: Integer) . words) . lines
        . map (\ x -> if x == ',' then ' ' else x) . filter (\ x -> x `notElem` ['-', '>']) <$> readFile "input/DayFiveInput.txt"
    print $ findAllIntersectingPointsNoDiag input
    print $ findAllIntersectingPoints input

initialBucket :: M.Map Integer Integer
initialBucket = M.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0)]

lanternFishSpawningSim :: Integer -> M.Map Integer Integer -> Integer
lanternFishSpawningSim 0 state = sum . map snd $ M.toList state
lanternFishSpawningSim days state = lanternFishSpawningSim (days - 1) (M.adjustWithKey (\_ val -> val + numberReadyToSpawn) 8 $ M.adjustWithKey (\_ val -> val + numberReadyToSpawn) 6 (updateState state <> initialBucket))
    where
        updateState = M.mapKeys (\x -> if x == 0 then 6 else x - 1)
        numberReadyToSpawn = state M.! 0


daySixSolutionIO :: IO ()
daySixSolutionIO = do
    input <- map (\x -> read x :: Integer) . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/DaySixInput.txt"
    let input' = M.fromListWith (+) (zip input (repeat 1)) <> initialBucket
    print $ lanternFishSpawningSim 80 input'
    print $ lanternFishSpawningSim 256 input'

crabLeastFuelConsumed :: [Integer] -> Integer
crabLeastFuelConsumed xs = minimum . map snd . M.toList $ M.mapWithKey (\k _ -> (sum . map (\x -> abs (x - k))) xs) uniquePositions
    where
        minVal = minimum xs
        maxVal = maximum xs
        uniquePositions = M.fromList $ zip [minVal..maxVal] (repeat 0)

crabLeastFuelConsumed' :: [Integer] -> Integer
crabLeastFuelConsumed' xs = minimum . map snd . M.toList $ M.mapWithKey (\k _ -> (sum . map (`sumAlgo` k)) xs) uniquePositions
    where
        minVal = minimum xs
        maxVal = maximum xs
        uniquePositions = M.fromList $ zip [minVal..maxVal] (repeat 0)
        sumAlgo k x = absVal * (absVal + 1) `div` 2
            where
                absVal = abs (x - k)

daySevenSolutionIO :: IO ()
daySevenSolutionIO = do
    input <- map (\x -> read x :: Integer) . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/DaySevenInput.txt"
    print $ crabLeastFuelConsumed input
    print $ crabLeastFuelConsumed' input