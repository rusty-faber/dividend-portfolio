module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (intercalate)
import Control.Exception (try, SomeException)
import Lib
    ( convertSampleXmlToCsv,
      convertXmlFileToCsv,
      writeCsvDocument,
      getTagString,
      getTagType,
      printFileContents,
      TagType(Child, Root) )

main :: IO ()
main = do
    args <- getArgs
    executeCommand args
    return ()
    
executeCommand :: [String] -> IO ()
executeCommand ["-e", inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand ["-m", inputFile, outputFile] = convertInToOut convertDividendToCsv inputFile outputFile
executeCommand [inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand [filePath] = printFile printFileContents filePath
executeCommand _ = putStrLn "Usage: program <filename> or program <inputFile> <outputFile>"

convertDividendToCsv :: String -> String -> IO ()
convertDividendToCsv inputFile outputFile = do
    let tagXML = [(Root, "ZLG00100"),
                             (Child, "ZLG00110"), (Child, "ZLG00120"), (Child, "ZLG00130")
                             , (Child, "ZLG00140"), (Child, "ZLG00150"), (Child, "ZLG00160")
                             , (Child, "ZLG00170"), (Child, "ZLG00180"), (Child, "ZLG00190")]
    putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ " as CSV."
    -- Create the CSV header from the child tags
    let header = intercalate "," (map getTagString (filter (\t -> getTagType t == Child) tagXML))
    -- Try to convert the XML file to CSV
    result <- try (convertXmlFileToCsv inputFile tagXML) :: IO (Either SomeException [String])
    case result of
        Left ex -> putStrLn $ "Error processing XML file: " ++ show ex
        Right csvData -> writeCsvDocument outputFile header csvData
    return ()


convertInToOut :: (String -> String -> IO ()) -> String -> String -> IO ()
convertInToOut printFunc inputFile outputFile = do
    inputExists <- doesFileExist inputFile
    outputExists <- doesFileExist outputFile
    if inputExists
        then if not outputExists
            then printFunc inputFile outputFile
            else putStrLn $ "Error: Output file " ++ outputFile ++ " already exists."
        else putStrLn $ "Error: Input file " ++ inputFile ++ " does not exist."
    return ()

printFile :: (String -> IO ()) -> String -> IO ()
printFile printFunc filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then printFunc filePath
        else putStrLn $ "Error: File " ++ filePath ++ " does not exist."
    return ()