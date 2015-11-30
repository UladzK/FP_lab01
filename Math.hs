module Math
(
	euclideanDistance,
	hammingDistance,	
	multNumOnVector,	
	subtractMatrices,
	vectorsSum,
	matrixNorm,
	transposeMatrix,
	rollFraction,
	matrixMax,
	matrixMin
) where

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance xs ys = sqrt . sum . map (^2) $ zipWith (-) xs ys

hammingDistance :: (Floating a) => [a] -> [a] -> a
hammingDistance xs ys = sum . map (abs) $ zipWith (-) xs ys

multNumOnVector :: (Num a) => a -> [a] -> [a]
multNumOnVector n xs = map (*n) xs

subtractMatrices :: (Num a) => [[a]] -> [[a]] -> [[a]]
subtractMatrices = zipWith (zipWith (-))

vectorsSum :: Num a => [[a]] -> [a]
vectorsSum = foldl1 (zipWith (+))

matrixNorm :: (Num a, Ord a) => [[a]] -> a
matrixNorm = matrixMax . absMatrix

matrixMax :: (Num a, Ord a) => [[a]] -> a
matrixMax = maximum . map (maximum)

matrixMin :: (Num a, Ord a) => [[a]] -> a
matrixMin = minimum . map (minimum)

absMatrix :: (Num a) => [[a]] -> [[a]]
absMatrix = map (map (abs))

transposeMatrix :: (Num a) => [[a]] -> [[a]]
transposeMatrix [[]] = []
transposeMatrix ([]:_) = []
transposeMatrix xs = map (head) xs : transposeMatrix (map (tail) xs)

rollFraction :: (Fractional a) => a -> a
rollFraction = (/) 1