module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            inputExists <- doesFileExist inputFile
            if inputExists
                then convertXmlToCsv inputFile outputFile
                else putStrLn $ "Error: Input file " ++ inputFile ++ " does not exist."
        [filePath] -> do
            fileExists <- doesFileExist filePath
            if fileExists
                then printFileContents filePath
                else putStrLn $ "Error: File " ++ filePath ++ " does not exist."
        _ -> putStrLn "Usage: program <filename> or program <inputFile> <outputFile>"
