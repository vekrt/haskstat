module Haskstat where

import Data.List
import Data.Maybe

floatLength :: Fractional a => [b] -> a
floatLength xs = fromIntegral $ length xs

mean :: Fractional a => [a] -> a
mean xs = (foldl' (+) 0 xs) / floatLength xs

trimmedMean :: (Ord a, Fractional a) => [a] -> Int -> a
trimmedMean xs 0 = mean xs
trimmedMean xs m = mean $ take (n-2*m) $ drop m (sort xs)
    where
        n = length xs

var :: Fractional a => [a] -> a
var xs =  (x2Sum/n - (xSum/n)^2)
    where
        (xSum, x2Sum) = foldl' (\(f,s) x -> (f+x,s+x^2)) (0,0) xs
        n = floatLength xs

varSample :: Fractional a => [a] -> a
varSample xs = n/(n-1) * var xs
    where 
        n = floatLength xs

std :: Floating a => [a] -> a
std x = sqrt $ var x

stdSample :: Floating a => [a] -> a
stdSample x = sqrt $ varSample x

skewness :: Floating a => [a] -> a
skewness xs = (mu3/n) / (mu2/n)**(1.5)
    where
        mu = mean xs
        (mu2, mu3) = foldl' (\(s,t) x -> (s + (x-mu)^2, t + (x-mu)^3)) (0, 0) xs
        n = floatLength xs

quartileSkewness :: (RealFrac a, Floating a) => [a] -> a
quartileSkewness xs = (q3 + q1 - 2.0*q2)/(q3 - q1)
    where
        q1 = percentile xs 25
        q2 = median xs
        q3 = percentile xs 75
    
octileSkewness :: (RealFrac a, Floating a) => [a] -> a
octileSkewness xs = (q875 + q125 - 2.0*q2)/(q875 - q125)
    where
        q125 = percentile xs 12.5
        q2 = median xs 
        q875 = percentile xs 87.5
    
substractValue :: Floating a => [a] -> a -> [a]
substractValue (x:xs) v = abs(x-v):(substractValue xs v)
substractValue [] v = []

mad :: (RealFrac a, Floating a) => [a] -> a
mad xs = median dev
    where
        dev = substractValue xs $ median xs 
    
--medcouple :: (Ord a, Floating a, RealFrac a) => [a] -> a
--medcouple xs = med 
--	where
--		sorted_xs = reverse $ sort xs
--		med = median sorted_xs
--		zs = sorted_xs - med
--		h_zi_zj = [
--		x_plus = filter (med<) xs
--		x_minus = filter (med>) xs

rank :: (Enum a, Num a, Ord a, Fractional a) => [a] -> [a]
rank xs = fromJust <$> map (\k -> lookup k pairs) xs
    where
            g = groupBy (\a b -> fst a == fst b) $ zip (sort xs) [1..]
            r = map (\k -> mean $ map snd k) g
            pairs = zip (nub (sort xs)) r
        
kurtosis :: Floating a => [a] -> a
kurtosis xs = n * mu4 / mu2^2
    where 
        mu = mean xs
        (mu2, mu4) = foldl' (\(s, f) x -> (s + (x-mu)^2, f + (x-mu)^4)) (0, 0) xs
        n = floatLength xs
    
robustKurtosis :: (RealFrac a, Floating a) => [a] -> a
robustKurtosis xs = ((q875 - q625) + (q375 - q125))/(q75 - q25)
    where
        q125 = percentile xs 12.5
        q25  = percentile xs 25
        q375 = percentile xs 37.5
        q625 = percentile xs 62.5
        q75  = percentile xs 75
        q875 = percentile xs 87.5
    
percentile :: (Ord a, Fractional a, RealFrac a) => [a] -> a -> a
percentile xs 0 = minimum xs
percentile xs 100 = maximum xs
percentile xs perc = (xSorted!!idx) * (1-fraction) + (xSorted!!(idx+1)) * (fraction)
    where 
        xSorted = sort xs
        perc' = ( perc)/100.0
        n = floatLength xs
        idx = floor (perc'*(n-1)) :: Int
        fraction = perc'*(n-1) - fromIntegral idx
    
median :: (Ord a, Fractional a, RealFrac a) => [a] -> a
median xs = percentile xs 50

data Summary a = Summary {
        min_ :: a,
        p25 :: a,
        med :: a,
        p75 :: a,
        max_ :: a
        } deriving (Show)

summary :: (Ord a, Fractional a, RealFrac a) => [a] -> Summary a
summary xs = Summary {
        min_ = minimum xs, 
        p25 = percentile xs 25,  
        med = median xs, 
        p75 = percentile xs 75,
        max_ = maximum xs
        }

count :: (Ord a, Num a) => [a] -> [(a,Int)]
count x = zip (nub x_sorted) (map length (group x_sorted))
    where
        x_sorted = sort x
    
binning :: (Ord a, Fractional a) => [a] -> Int -> [((a,a), Int)]
binning [] _ = []
binning x nbrBin = zip step $ map length filters
    where 
        step = steps ((maxX - minX) / fromIntegral nbrBin) minX maxX
        maxX = maximum x
        minX   = minimum x
        bounds  = map (\(a,b) -> (&&) <$> (>=a) <*> (<b)) (init step) ++ 
                     [(\(a,b) -> (&&) <$> (>=a) <*> (<=b)) (last step)]
        filters = map (flip filter x) bounds

steps :: (Ord a, Num a) => a -> a -> a -> [(a,a)]
steps step start stop  
    | start < (stop-step) = (start, start+step) : steps step (start+step) stop
    | otherwise = [(start,stop)]

e :: Floating a => a
e = 2.718281828459045

jarqueBera :: Floating a => [a] -> (a, a)
jarqueBera xs = (value, e**(-0.5*value))
    where
        n = fromIntegral $ length xs
        mu = mean xs
        (mu2, mu3, mu4) = foldl' (\(s,t,f) x -> (s+(x-mu)^2, t+(x-mu)^3, f+(x-mu)^4)) (0,0,0) xs
        s = (mu3/n)/(mu2/n)**1.5
        k = n * mu4 /mu2^2
        value = n / 6.0 * (s^2 + 0.25*(k - 3.0)^2)
