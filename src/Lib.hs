module Lib (
    dayOneSolutionIO
,   dayTwoSolutionIO
,   getDisplacementProductFromMovementWithAim
) where

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