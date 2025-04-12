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

data TagType = Root | Child deriving (Show, Eq)
type Tag = (TagType, String)

getTagType :: Tag -> TagType
getTagType (Child, _) = Child
getTagType (Root, _) = Root 
getTagString :: Tag -> String
getTagString (Root, tagName) = tagName
getTagString (Child, tagName) = tagName

-- Function to print the contents of a file
printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content

-- Function to convert a sample XML file to CSV
convertSampleXmlToCsv :: FilePath -> FilePath -> IO ()
convertSampleXmlToCsv inputFile outputFile = do
    let tagXML = [(Root,"Stock"),
                    (Child,"Symbol"),
                    (Child,"Name"),
                    (Child,"Shares"),
                    (Child,"PurchasePrice"),
                    (Child,"CurrentPrice")]
    let header = intercalate "," (map snd (filter (\t -> getTagType t == Child) tagXML))
    result <- try (convertXmlToCsvRunX inputFile tagXML) :: IO (Either SomeException [String])
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> writeCsvDocument outputFile header csvData

-- Function to convert XML to CSV using HXT
convertXmlToCsvRunX :: FilePath -> [Tag] -> IO [String]
convertXmlToCsvRunX inputFile tagXML = do
    csvData <- runX $ readDocument [withValidate no, withRemoveWS yes] inputFile >>> 
                      processXmlToCsv tagXML
    return csvData

-- Function to write CSV data to a file
writeCsvDocument :: FilePath -> String -> [String] -> IO ()
writeCsvDocument outputFile header csvData = do
    writeFile outputFile (unlines (header : csvData))
    putStrLn "Debugging CSV Content:"
    mapM_ putStrLn (header : csvData)  -- Debug output of CSV content
    return ()

-- Function to process XML and extract data as CSV
processXmlToCsv :: ArrowXml a => [Tag] -> a XmlTree String
processXmlToCsv tagXML =
    case listToMaybe tagXML of
        Nothing -> constA "No tags specified" -- Error message for empty list
        Just (_, rootTagName) -> deep (isElem >>> hasName rootTagName) >>> -- Extract root tag name using pattern matching
            proc rootElement -> do
                let childTags = map snd (filter (\t -> getTagType t == Child) tagXML) -- Get list of child tag names
                childrenTexts <-  getXMLChildrenText ["Symbol","Name"] -< rootElement
                returnA -< childrenTexts

getXMLChildrenText :: ArrowXml a => [String] -> a XmlTree String
getXMLChildrenText childTags =
    proc rootElement -> do
        childrenTexts <- listA(foldl1 (<+>) (map (\tag ->
            getChildren >>>
            isElem >>>
            hasName tag >>>
            xshow getChildren) childTags)) -< rootElement
        returnA -< intercalate "," childrenTexts