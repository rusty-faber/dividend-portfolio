module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Lib

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content
printFileContents _ = putStrLn "File not found."

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> printFileContents fileName
        _          -> putStrLn "Usage: program <filename>"
