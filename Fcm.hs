module Fcm
(
	classify,
	VectorDistance(..),
	StartMethod(..)
) where

import Math
import System.Random

data VectorDistance = Euclidean | Hamming
	deriving (Show, Read, Eq)

data StartMethod = Weights | Clusters
	deriving (Show, Read, Eq)

splitVector :: Int -> [a] -> [[a]]
splitVector _ [] = []
splitVector n xs = take n xs : splitVector n (drop n xs)

generateRandomMatrix :: StdGen -> Int -> Int -> (Float, Float) -> [[Float]]
generateRandomMatrix g nv nc r = splitVector nc (generateRandomVector g r size)
	where 
		size = nv * nc

generateRandomVector :: StdGen -> (Float, Float) -> Int -> [Float]
generateRandomVector g r n = take n $ randomRs r g :: [Float]

calculateClusters :: [[Float]] -> [[Float]] -> [[Float]]
calculateClusters xs ws = map (getCluster xs) (transposeMatrix ws)
 	where
  		getCluster x cws = multNumOnVector (rollFraction $ sum cws) (vectorsWeightSum cws x)
  		vectorsWeightSum cws xs = vectorsSum $ zipWith (\w x -> multNumOnVector (w^m) x) cws xs
  		m = 2

calculateWeights :: (Floating a) => ([a] -> [a] -> a) -> [[a]] -> [[a]] -> [[a]]
calculateWeights d xs cs = map (getWeights) xs
	where
		getWeights x = map (getRelativeWeight x) cs
		getRelativeWeight x v = rollFraction $ sum $ map (\c -> (d x v / d x c)^2) cs

classify' :: ([Float] -> [Float] -> Float) -> [[Float]] -> Float -> [[Float]] -> [[Float]]
classify' d ws e xs = 
	if (matrixNorm $ subtractMatrices nws ws) > e
		then classify' d nws e xs
		else nws
	where nws = calculateWeights d xs $ calculateClusters xs ws

classify :: StartMethod -> VectorDistance -> Int -> Float -> [[Float]] -> [[Float]]
classify _ _ _ _ [[]] = []
classify sm vd nc e xs = getClassifyDistance vd initMatrix e xs	
	where
		initMatrix = getClassifyInitMatrix sm xs nc

getClassifyInitMatrix :: StartMethod -> [[Float]] -> Int -> [[Float]]
getClassifyInitMatrix sm xs nc
	| sm == Weights = generateFcmInitMatrix (mkStdGen 0) numberOfVectors nc
	| sm == Clusters = calculateWeights euclideanDistance xs $ getRandomClusters (mkStdGen 100) xs nc
	| otherwise = error "Start method is not supported"
		where numberOfVectors = length xs		

getClassifyDistance :: VectorDistance -> ([[Float]] -> Float -> [[Float]] -> [[Float]])
getClassifyDistance d
	| d == Hamming = classify' hammingDistance
	| d == Euclidean = classify' euclideanDistance
	| otherwise = error "Distance method is not supported"

getRandomClusters :: StdGen -> [[Float]] -> Int -> [[Float]]
getRandomClusters g xs nc = [xs !! i | i <- take nc $ randomRs (0, length xs - 1) g]

generateFcmInitMatrix :: StdGen -> Int -> Int -> [[Float]]
generateFcmInitMatrix g nv nc = map (adduct) (generateRandomMatrix g nv nc (0, 1))
	where 
		adduct xs = sub ((sum xs - 1) / fromIntegral nc) xs
		sub s xs = map (subtract s) xs