import Data.List

mean :: Fractional a => [a] -> a
mean x = (foldl' (+) 0 x) / fromIntegral (length x)

var :: Fractional a => [a] -> a
var xs =  (x2Sum/n - (xSum/n)^2)
              where
	        (xSum, x2Sum) = foldl' (\(f,s) x -> (f+x,s+x^2)) (0,0) xs
		n = fromIntegral $ length xs

varSample :: Fractional a => [a] -> a
varSample xs = n/(n-1) * var xs
                where 
		  n = fromIntegral $ length xs

std :: Floating a => [a] -> a
std x = sqrt $ var x

stdSample :: Floating a => [a] -> a
stdSample x = sqrt $ varSample x

percentile :: (Ord a, Fractional a, RealFrac a) => [a] -> a -> a
percentile x 0 = minimum x
percentile x 100 = maximum x
percentile x perc = (xSorted!!idx) * (1-fraction) + (xSorted!!(idx+1)) * (fraction)
                    where 
		     xSorted = sort x
		     perc' = ( perc)/100.0
		     n = fromIntegral $ length x
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


jarqueBera :: Num a => [a] -> [b]
jarqueBera = undefined
