{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printFileContents,
      convertSampleXmlToCsv
    ) where

import Text.XML.HXT.Core
import Data.List (intercalate, uncons)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Exception (try, SomeException)

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content

convertSampleXmlToCsv :: FilePath -> FilePath -> IO ()
convertSampleXmlToCsv inputFile outputFile = do
    let tagXML = ["Stock", "Symbol", "Name", "Shares", "PurchasePrice", "CurrentPrice"]
    let header = intercalate "," (fromMaybe [] (snd <$> uncons tagXML))
    result <- try (convertXmlToCsvRunX inputFile tagXML) :: IO (Either SomeException [String])
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> writeCsvDocument outputFile header csvData

convertXmlToCsvRunX :: FilePath -> [String] -> IO [String]
convertXmlToCsvRunX inputFile tagXML = do
    csvData <- runX $ readDocument [withValidate no, withRemoveWS yes] inputFile >>> 
                      processXmlToCsv tagXML
    return csvData

writeCsvDocument :: FilePath -> String -> [String] -> IO ()
writeCsvDocument outputFile header csvData = do
    writeFile outputFile (unlines (header : csvData))
    putStrLn "Debugging CSV Content:"
    mapM_ putStrLn (header : csvData)  -- CSVの内容をデバッグ出力
    return ()

processXmlToCsv :: ArrowXml a => [String] -> a XmlTree String
processXmlToCsv tagXML = 
    case listToMaybe tagXML of
        Nothing -> constA "タグが指定されていません" -- 空リストの場合のエラーメッセージ
        Just rootTag -> deep (isElem >>> hasName rootTag) >>>
            proc stock -> do
                childrenXML <- getXMLChildrenText (drop 1 tagXML) -< stock
                returnA -< childrenXML

-- Helper function to retrieve the text of a child element
getXMLChildrenText :: ArrowXml a => [String] -> a XmlTree String
getXMLChildrenText tagXML =
    proc stock -> do
        texts <- listA (foldl1 (<+>) (map (\tag -> 
            getChildren >>> 
            isElem >>> 
            hasName tag >>> 
            xshow getChildren) tagXML)) -< stock
        returnA -< intercalate "," texts

-- getChildText :: ArrowXml a => String -> a XmlTree String
-- getChildText tagName = 
--     getChildren >>> isElem >>> hasName tagName >>> xshow getChildren