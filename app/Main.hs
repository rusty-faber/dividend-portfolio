module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            putStrLn $ "File contents:\n" ++ content
        _          -> putStrLn "Usage: program <filename>"
