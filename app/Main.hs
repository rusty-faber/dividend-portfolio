module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Lib

main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then printFileContents (head args)
        else putStrLn "Usage: program <filename>"
