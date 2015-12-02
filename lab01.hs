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

	let nc = args !! 0
	let pr = args !! 1
	let fileName = args !! 2

	let numberOfClusters = readMaybe nc :: Maybe Int
	if (isNothing numberOfClusters)
		then
			error $ "Incorrect number of clusters: " ++ nc
		else
			return ()

	let precision = readMaybe pr :: Maybe Float
	if (isNothing precision)
		then
			error $ "Incorrect precision of classify: " ++ pr
		else
			return ()

	return (fromJust numberOfClusters, fromJust precision, fileName, stringsToMap $ drop 3 args)

main = do

	(numberOfClusters, precision, fileName, extraArgsMap) <- parseArgs

	let csvSeparator = Map.findWithDefault "," "-fsp" extraArgsMap
	
	let startMethod = read (Map.findWithDefault "Weights" "-sm" extraArgsMap) :: StartMethod
	let distanceMethod = read (Map.findWithDefault "Euclidean" "-d" extraArgsMap) :: VectorDistance

	let csvIgnoreFirstRow = readMaybe (Map.findWithDefault "False" "-ifr" extraArgsMap) :: Maybe Bool
	let csvIgnoreLastColumn = readMaybe (Map.findWithDefault "False" "-ilc" extraArgsMap) :: Maybe Bool
	let csvIgnoreFirstColumn = readMaybe (Map.findWithDefault "False" "-ifc" extraArgsMap) :: Maybe Bool

	if (isNothing $ sequenceA [csvIgnoreFirstRow, csvIgnoreLastColumn, csvIgnoreFirstColumn])
		then
			error "Incorrect ignore flags for CSV. See --help for usage"
		else
			return ()

	csvParsed <- parseCSV csvSeparator
		(fromJust csvIgnoreFirstRow, fromJust csvIgnoreFirstColumn, fromJust csvIgnoreLastColumn) fileName

	let vectors = map (map (\x -> read x :: Float)) csvParsed
	let classifyResult = classify startMethod distanceMethod numberOfClusters precision vectors

	printResult (Map.lookup "-out" extraArgsMap) classifyResult