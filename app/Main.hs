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
executeCommand [inputFile, outputFile] = do
    inputExists <- doesFileExist inputFile
    if inputExists
        then convertSampleXmlToCsv inputFile outputFile
        else putStrLn $ "Error: Input file " ++ inputFile ++ " does not exist."
executeCommand [filePath] = do
            fileExists <- doesFileExist filePath
            if fileExists
                then printFileContents filePath
                else putStrLn $ "Error: File " ++ filePath ++ " does not exist."
executeCommand _ = putStrLn "Usage: program <filename> or program <inputFile> <outputFile>"
