module Main (main) where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> convertXml2Csv inputFile outputFile
        [filePath] -> printFileContents filePath
        _ -> putStrLn "Usage: program <filename> or program <inputFile> <outputFile>"
