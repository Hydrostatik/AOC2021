module Lib (
    dayOneSolutionIO
,   dayTwoSolutionIO
,   dayThreeSolutionIO
,   oxygenGeneratorRating
,   co2ScrubberRating
) where

import Data.Char
import Data.List

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