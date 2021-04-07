module Main where 

import Haskstat

sample:: Num a => [a]
sample = [2, 8, 0, 4, 1, 9, 9, 0, 12]

isPassed :: String -> Bool -> IO ()
isPassed msg True = putStrLn $ msg ++ " passed"
isPassed msg False  = putStrLn $ msg ++ " failed"

myFloor :: (RealFrac a, Fractional a, Num a) => a -> Int -> a
myFloor x exp = fromIntegral (floor (x * prec)) / prec
        where 
            prec = 10^exp

main :: IO ()
main = do 
    "Test mean on sample:" `isPassed` (mean sample == 5) 
    "Test trimmedMean 0 on sample:" `isPassed` (trimmedMean sample 0 == 5)
    "Test trimmedMean 1 on sample:" `isPassed` (myFloor (trimmedMean sample 1) 3 == 4.714)
    "Test trimmedMean 2 on sample:" `isPassed` (myFloor (trimmedMean sample 2) 3 == 4.8)
    "Test trimmedMean 3 on sample:" `isPassed` (myFloor (trimmedMean sample 3) 3 == 4.666)
    "Test var sample:" `isPassed` (myFloor (var sample) 3 == 18.444)
    "Test std sample:" `isPassed` (myFloor (std sample) 3 == 4.294)
    "Test skewness sample:" `isPassed` (myFloor (skewness sample) 3 == 0.218)
