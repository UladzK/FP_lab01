import Prelude hiding (catch)
import Fcm
import System.Random
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.List.Split as Split
import Control.Exception as Exception

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

stringsToMap :: (Ord a) => [[a]] -> Map.Map [a] [a]
stringsToMap [] = Map.empty
stringsToMap xs = Map.fromList $ stringsToMap' xs
	where
		stringsToMap' ([]) = []
		stringsToMap' (x:[]) = (x, []) : []
		stringsToMap' (x:xs) = (x, head xs) : stringsToMap' (tail xs)

printResult :: (Show a) => Maybe String -> [a] -> IO ()
printResult fileName result
	| Maybe.isNothing fileName = putStrLn viewResult
 	| otherwise = writeFile (fromJust fileName) viewResult
 	where viewResult = unlines . map (show) $ result

cutOff :: Bool -> ([a] -> [a]) -> [a] -> [a]
cutOff p f xs
	| p == True = f xs
	| otherwise = xs

splitWithIgnore :: (Bool, Bool) -> String -> String -> [String]
splitWithIgnore ifs s = 
	ignoreLastColumn (snd ifs)
	. ignoreFirstColumn (fst ifs)
	. Split.splitOn s
		where 
			ignoreFirstColumn ifl = cutOff ifl tail
			ignoreLastColumn ifl = cutOff ifl init

ioHandler :: IOException -> IO String
ioHandler e
	| isDoesNotExistError e = error "Vectors input file does not exist"
	| otherwise = ioError e

parseCSV :: String -> (Bool, Bool, Bool) -> FilePath -> IO [[String]]
parseCSV s ifs fp = do
	fc <- readFile fp `catch` ioHandler
	return $ 
		map (splitWithIgnore (second ifs, third ifs) s)
		. ignoreFirstRow (first ifs)
		. lines $ fc
			where ignoreFirstRow ifl = cutOff ifl tail				

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
				[(x, "")] -> Just x
				_ -> Nothing

parseArgs :: IO (Int, Float, String, Map.Map String String)
parseArgs = do
	args <- getArgs

	if (length args < 3)
		then 
			error "Not enough arguments. See --help for usage"
		else
			return ()

	let numberOfClusters = readMaybe (args !! 0) :: Int
	let precision = readMaybe (args !! 1) :: Float
	let fileName = Just args !! 2

	--if (isNothing $ sequenceA [numberOfClusters, precision, fileName])
	--	then
	--		error "Incorrect mandatory arguments. See --help for usage"
	--	else
	--		return ()
	--return (numberOfClusters, precision, fileName, stringsToMap $ drop 3 args)

main = do

	(numberOfClusters, precision, fileName, extraArgsMap) <- parseArgs

	let csvSeparator = Map.findWithDefault "," "-fsp" extraArgsMap
	
	let startMethod = read (Map.findWithDefault "Weights" "-sm" extraArgsMap) :: StartMethod
	let distanceMethod = read (Map.findWithDefault "Euclidean" "-d" extraArgsMap) :: VectorDistance
	let csvIgnoreFirstRow = read (Map.findWithDefault "False" "-ifr" extraArgsMap) :: Bool
	let csvIgnoreLastColumn = read (Map.findWithDefault "False" "-ilc" extraArgsMap) :: Bool
	let csvIgnoreFirstColumn = read (Map.findWithDefault "False" "-ifc" extraArgsMap) :: Bool

	csvParsed <- parseCSV csvSeparator
		(csvIgnoreFirstRow, csvIgnoreFirstColumn, csvIgnoreLastColumn) fileName

	let vectors = map (map (\x -> read x :: Float)) csvParsed
	let classifyResult = classify startMethod distanceMethod numberOfClusters precision vectors

	printResult (Map.lookup "-out" extraArgsMap) classifyResult