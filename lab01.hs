{-# LANGUAGE DeriveDataTypeable #-}

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
import System.Console.CmdArgs
import Control.Monad

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

fileIOHandler :: IOException -> IO String
fileIOHandler e
    | isDoesNotExistError e = error "Vectors input file does not exist"
    | otherwise = ioError e

parseCSV :: String -> (Bool, Bool, Bool) -> FilePath -> IO [[String]]
parseCSV s ifs fp = do
    fc <- readFile fp `catch` fileIOHandler
    return $ 
        map (splitWithIgnore (second ifs, third ifs) s)
        . ignoreFirstRow (first ifs)
        . lines $ fc
            where ignoreFirstRow ifl = cutOff ifl tail              

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
                [(x, "")] -> Just x
                _ -> Nothing

parseArgs :: IO InputSettings
parseArgs = do
    args <- cmdArgs defaultArgs

    let nc = numberOfClusters args
    let pr = precision args
    let fn = inFile args

    if (nc <= 0)
        then
            error $ "Incorrect number of clusters. See --help for usage"
        else
            return ()

    if (pr <= 0)
        then
            error $ "Incorrect precision of classify. See --help for usage"
        else
            return ()

    cmdArgs defaultArgs

data InputSettings = InputSettings 
                    {
                        numberOfClusters :: Int,
                        precision :: Float,
                        inFile :: String,
                        outFile :: String,
                        csvSeparator :: String,
                        startMethod :: StartMethod,
                        distanceMethod :: VectorDistance,
                        ignoreFirstRow :: Bool,
                        ignoreLastColumn :: Bool,
                        ignoreFirstColumn :: Bool
                    }
                    deriving (Show, Read, Data, Typeable)

defaultArgs = InputSettings
        {
            numberOfClusters = -1,
            precision = -1,
            inFile = "",
            outFile = "",
            csvSeparator = ",",
            startMethod = Weights,
            distanceMethod = Euclidean,
            ignoreFirstRow = False,
            ignoreLastColumn = False,
            ignoreFirstColumn = False
        }

main = do
    parsedSettings <- parseArgs

    let inFileName = inFile parsedSettings
    let nc = numberOfClusters parsedSettings
    let sp = csvSeparator parsedSettings
    let sm = startMethod parsedSettings
    let dm = distanceMethod parsedSettings
    let ifr = ignoreFirstColumn parsedSettings
    let ilc = ignoreLastColumn parsedSettings
    let ifc = ignoreFirstColumn parsedSettings

    let outFileName = case (outFile parsedSettings) of
                        "" -> Nothing
                        x -> Just x

    let pr = precision parsedSettings

    csvParsed <- parseCSV sp (ifr, ifc, ilc) inFileName

    let maybeVectors = sequence $ map (sequenceA) $ map (map (\x -> readMaybe x :: Maybe Float)) csvParsed
    
    let vectors = case (isNothing maybeVectors) of
                True -> error "Incorrect vectors in vectors file"
                False -> fromJust (maybeVectors)

    let classifyResult = classify sm dm nc pr vectors

    printResult outFileName classifyResult