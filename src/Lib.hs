{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printFileContents,
      convertXmlToCsv
    ) where

import Text.XML.HXT.Core
import Data.List (intercalate)
import Control.Exception (try, SomeException)

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content

convertXmlToCsv :: FilePath -> FilePath -> IO ()
convertXmlToCsv inputFile outputFile = do
    result <- try (convertXmlToCsvRunX inputFile) :: IO (Either SomeException [String])
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> writeCsvDocument outputFile csvData

convertXmlToCsvRunX :: FilePath -> IO [String]
convertXmlToCsvRunX inputFile = do
    csvData <- runX $ readDocument [withValidate no, withRemoveWS yes] inputFile >>> processXmlToCsv
    return csvData

writeCsvDocument :: FilePath -> [String] -> IO ()
writeCsvDocument outputFile csvData = do
    let header = "Symbol,Name,Shares,PurchasePrice,CurrentPrice"
    writeFile outputFile (unlines (header : csvData))
    putStrLn "Debugging CSV Content:"
    mapM_ putStrLn csvData  -- CSVの内容をデバッグ出力
    return ()

processXmlToCsv :: ArrowXml a => a XmlTree String
processXmlToCsv =
    deep (isElem >>> hasName "Stock") >>>
    proc stock -> do
        symbol <- getChildText "Symbol" -< stock
        name <- getChildText "Name" -< stock
        shares <- getChildText "Shares" -< stock
        purchasePrice <- getChildText "PurchasePrice" -< stock
        currentPrice <- getChildText "CurrentPrice" -< stock
        
        returnA -< intercalate "," [symbol, name, shares, purchasePrice, currentPrice]

-- Helper function to retrieve the text of a child element
getChildText :: ArrowXml a => String -> a XmlTree String
getChildText tagName =
    getChildren >>> isElem >>> hasName tagName >>> xshow getChildren