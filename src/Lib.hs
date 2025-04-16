{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( convertXmlFileToCsv,
      processXmlToCsv,
      convertSampleXmlToCsv,
      printFileContents,
      getTagType,
      getTagString,
      TagType(..),
      Tag,
    ) where

import Text.XML.HXT.Core
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Control.Exception (try, SomeException)

data TagType = Root | Child deriving (Show, Eq)
type Tag = (TagType, String)

-- Function to get the type of a tag (Root or Child)
getTagType :: Tag -> TagType
getTagType (Child, _) = Child
getTagType (Root, _) = Root 

-- Function to get the string value of a tag
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
    -- Create the CSV header from the child tags
    let header = intercalate "," (map getTagString (filter (\t -> getTagType t == Child) tagXML))
    -- Try to convert the XML file to CSV
    result <- try (convertXmlFileToCsv inputFile tagXML) :: IO (Either SomeException [String])
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> writeCsvDocument outputFile header csvData

-- Function to convert XML to CSV using HXT
convertXmlFileToCsv :: FilePath -> [Tag] -> IO [String]
convertXmlFileToCsv inputFile tagXML = do
    runX (readDocument [withValidate no, withRemoveWS yes] inputFile >>> processXmlToCsv tagXML)

-- Function to process XML and extract data as CSV
processXmlToCsv :: ArrowXml a => [Tag] -> a XmlTree String
processXmlToCsv tagXML = do
    -- Extract child tag names
    let childTags = map getTagString (filter (\t -> getTagType t == Child) tagXML)
    case listToMaybe tagXML of
        Nothing -> constA "No tags specified" -- Error message for empty list
        Just (_, rootTagName) -> deep (isElem >>> hasName rootTagName) >>>
            proc rootElement -> do
                -- Extract text from child elements
                childrenTexts <-  getXMLChildrenText childTags -< rootElement
                returnA -< childrenTexts

-- Function to extract text from child elements based on tag names
getXMLChildrenText :: ArrowXml a => [String] -> a XmlTree String
getXMLChildrenText childTags =
    proc rootElement -> do
        -- Extract text for each child tag
        childrenTexts <- listA(foldl1 (<+>) (map (\tag ->
            getChildren >>>
            isElem >>>
            hasName tag >>>
            xshow getChildren) childTags)) -< rootElement
        -- Combine the extracted texts into a single CSV row
        returnA -< intercalate "," childrenTexts

-- Function to write CSV data to a file
writeCsvDocument :: FilePath -> String -> [String] -> IO ()
writeCsvDocument outputFile header csvData = do
    writeFile outputFile (unlines (header : csvData))
    putStrLn "Debugging CSV Content:"
    mapM_ putStrLn (header : csvData)  -- Debug output of CSV content
