{-# LANGUAGE Arrows #-}

module Lib
    ( printFileContents,
      convertXml2Csv
    ) where

import Control.Arrow
import Text.XML.HXT.Core
import Data.List (intercalate)
import Debug.Trace as Debug

printFileContents :: FilePath -> IO ()
printFileContents filePath = do
    content <- readFile filePath
    putStrLn content

convertXml2Csv :: FilePath -> FilePath -> IO ()
convertXml2Csv inputFile outputFile = do
    debugData <- runX (readDocument [withValidate no, withInputEncoding utf8] inputFile >>> writeDocumentToString [])
    putStrLn "Debugging XML Content:"
    mapM_ putStrLn debugData  -- XMLの内容をデバッグ出力
    csvData <- runX (readDocument [withValidate no, withInputEncoding utf8] inputFile >>> processXmlToCsv)
    let header = "Symbol,Name,Shares,PurchasePrice,CurrentPrice"
    writeFile outputFile (unlines (header : csvData))
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
        
        -- デバッグ用出力
        let debugInfo = unlines
                [ "Debugging Stock Element:"
                , "  Symbol: " ++ symbol
                , "  Name: " ++ name
                , "  Shares: " ++ shares
                , "  PurchasePrice: " ++ purchasePrice
                , "  CurrentPrice: " ++ currentPrice
                ]
     --   returnA -< Debug.trace debugInfo (intercalate "," [symbol, name, shares, purchasePrice, currentPrice])
        returnA -< intercalate "," [symbol, name, shares, purchasePrice, currentPrice]

-- 子要素のテキストを取得するヘルパー関数
getChildText :: ArrowXml a => String -> a XmlTree String
getChildText tagName =
    getChildren >>> isElem >>> hasName tagName >>> getText