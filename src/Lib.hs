module Lib
    ( printFileContents,
      convertXml2CSV
    ) where

import Text.XML.HXT.Core
import Data.List (intersperse)

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content
printFileContents _ = putStrLn "File not found."

convertXml2CSV :: FilePath -> FilePath -> IO ()
convertXml2CSV inputFile outputFile = do
    contents <- readFile inputFile
    let csvData = runLA (hread >>> processXml >>> writeCSV) contents
    writeFile outputFile (unlines (map (concat . intersperse ",") csvData))
  where
    processXml = deep (isElem >>> hasName "row") >>> getChildren >>> getText
    writeCSV = arr (concat . intersperse ",")
