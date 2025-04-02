module Lib
    ( printFileContents,
      convertXml2Csv
    ) where

import Text.XML.HXT.Core
import Data.List (intersperse)

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content
printFileContents _ = putStrLn "File not found."

convertXml2Csv :: FilePath -> FilePath -> IO ()
convertXml2Csv inputFile outputFile = do
    csvData <- runX (readDocument [withValidate no] inputFile >>> processXmlToCsv)
    writeFile outputFile (unlines csvData)
    return ()

processXmlToCsv :: ArrowXml a => a XmlTree String
processXmlToCsv =
    deep (isElem >>> hasName "record") >>>  -- Adjust "record" to match your XML structure
    listA (getChildren >>> getText) >>>
    arr (concat . intersperse ",")


