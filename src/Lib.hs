module Lib ( 
    dayOneSolutionIO
,   compareInWindow
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