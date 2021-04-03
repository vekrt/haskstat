import Data.List

floatLength :: Fractional a => [b] -> a
floatLength xs = fromIntegral $ length xs

mean :: Fractional a => [a] -> a
mean x = (foldl' (+) 0 x) / fromIntegral (length x)

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

kurtosis :: Floating a => [a] -> a
kurtosis xs = n * mu4 / mu2^2
	where 
		mu = mean xs
		(mu2, mu4) = foldl' (\(s, f) x -> (s + (x-mu)^2, f + (x-mu)^4)) (0, 0) xs
		n = floatLength xs

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
                  where step    = steps ((maxX - minX) / fromIntegral nbrBin) minX maxX
		        maxX   = maximum x
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
