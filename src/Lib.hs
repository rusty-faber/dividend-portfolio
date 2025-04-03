{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printFileContents,
      convertXml2Csv
    ) where

import Control.Arrow
import Control.Category
import Text.XML.HXT.Core
import Data.List (intercalate)
import Debug.Trace as Debug
import Control.Exception (try, SomeException)

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content

convertXml2Csv :: FilePath -> FilePath -> IO ()
convertXml2Csv inputFile outputFile = do
    result <- try $
        runX (readDocument [withValidate no, withInputEncoding utf8] inputFile >>> processXmlToCsv) :: IO (Either SomeException [String])
    -- Debugging: Is an exception occurring during processing?
    -- デバッグ途中、処理途中で例外が起きている？
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> do
            let header = "Symbol,Name,Shares,PurchasePrice,CurrentPrice"
            writeFile outputFile (unlines (header : csvData))
            putStrLn "Debugging CSV Content:"
            mapM_ putStrLn csvData  -- CSVの内容をデバッグ出力
    return ()

processXmlToCsv :: ArrowXml a => a XmlTree String
processXmlToCsv =
    -- deep (isElem >>> hasName "Stock") >>> writeDocumentToString []
    deep (isElem >>> hasName "Stock") >>>
    proc stock -> do
        -- symbol <- (isElem >>> hasName "Symbol" >>> getText) -< stock
        symbol <- getChildText "Symbol" -< stock
        returnA -< "" ++ symbol ++ ","
    --    symbol <- getChildText "Symbol" -< stock
    --     name <- getChildText "Name" -< stock
    --     shares <- getChildText "Shares" -< stock
    --     purchasePrice <- getChildText "PurchasePrice" -< stock
    --     currentPrice <- getChildText "CurrentPrice" -< stock
        
    --     -- デバッグ用出力
    --     let debugInfo = unlines
    --             [ "Debugging Stock Element:"
    --             , "  Symbol: " ++ symbol
    --             , "  Name: " ++ name
    --             , "  Shares: " ++ shares
    --             , "  PurchasePrice: " ++ purchasePrice
    --             , "  CurrentPrice: " ++ currentPrice
    --             ]
    --  --   returnA -< Debug.trace debugInfo (intercalate "," [symbol, name, shares, purchasePrice, currentPrice])
    --     returnA -< intercalate "," [symbol, name, shares, purchasePrice, currentPrice]

-- 子要素のテキストを取得するヘルパー関数
getChildText :: ArrowXml a => String -> a XmlTree String
getChildText tagName =
    getChildren >>> isElem >>> hasName tagName >>> getText