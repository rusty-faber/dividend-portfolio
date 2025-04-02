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
    runX (readDocument [withValidate no] inputFile
          >>> processXmlToCsv
          >>> writeDocument [withOutputEncoding utf8] outputFile)
    return ()

processXmlToCsv :: ArrowXml a => a XmlTree String
processXmlToCsv =
    deep (isElem >>> hasName "record") >>>  -- Adjust "record" to match your XML structure
    listA (getChildren >>> getText) >>>
    arr (intersperse ',' >>> concat)

