# haskstat

Simple statistical package to learn a bit of Haskell

* mean: sample mean 
* trimmedMean: sample mean where *m* elements are removed on both side of the sorted array
* var: population variance
* varSample: sample variance
* std: population standard deviation 
* stdSample: sample standard deviation 
* skewness: skewness 
* quartileSkewness: skewness based on quartiles 
* octileSkewness: skewness based on octiles 
* mad: median absolute deviation 
* rank: rank of the elements in the array 
* kurtosis: kurtosis 
* robustKurtosis: kurtosis based on percentiles
* percentile: percentiles 
* median: median 
* count: counting the occurrence of the elements in the array  
* binning: simple histogram 
* jarqueBera: Jarque-Bera test of normality with *p-value* computed from Chi-Squared distribution 
* fft/ifft: radix-2 fast Fourier transform [Wiener-Khinchin](https://en.wikipedia.org/wiki/Autocorrelation#Wiener%E2%80%93Khinchin_theorem)
* autocorr: autocorrelation based on fft
* normalise: normalisation of the data, subtracting the mean and dividing by std
