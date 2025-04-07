module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Lib

main :: IO ()
main = do
    args <- getArgs
    executeCommand args
    return ()
    
executeCommand :: [String] -> IO ()
executeCommand ["-e", inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand [inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand [filePath] = printFile printFileContents filePath
executeCommand _ = putStrLn "Usage: program <filename> or program <inputFile> <outputFile>"

convertInToOut :: (String -> String -> IO ()) -> String -> String -> IO ()
convertInToOut printFunc inputFile outputFile = do
    inputExists <- doesFileExist inputFile
    outputExists <- doesFileExist outputFile
    if inputExists
        then if not outputExists
            then printFunc inputFile outputFile
            else putStrLn $ "Error: Output file " ++ outputFile ++ " already exists."
        else putStrLn $ "Error: Input file " ++ inputFile ++ " does not exist."
    return ()

printFile :: (String -> IO ()) -> String -> IO ()
printFile printFunc filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then printFunc filePath
        else putStrLn $ "Error: File " ++ filePath ++ " does not exist."
    return ()