module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (intercalate)
import Data.Either (isLeft, fromLeft, fromRight)
import Control.Exception (try, SomeException)
import Lib

main :: IO ()
main = do
    args <- getArgs
    executeCommand args
    return ()
    
executeCommand :: [String] -> IO ()
executeCommand ["-s", inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand ["-d", inputFile, outputFile] = convertInToOut convertDividendToCsv inputFile outputFile
executeCommand [inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand [filePath] = printFile printFileContents filePath
executeCommand _ = putStrLn $
    "Usage: program [options] <input_file> [output_file]\n" ++
    "\n" ++
    "Options:\n" ++
    "  -s <input_file> <output_file>\n" ++
    "      Convert sample XML data from <input_file> to CSV and save to <output_file>.\n" ++
    "\n" ++
    "  -d <input_file> <output_file>\n" ++
    "      Convert dividend data from <input_file> to CSV and save to <output_file>.\n" ++
    "\n" ++
    "  <input_file> <output_file>\n" ++
    "      (Default) Convert sample XML data from <input_file> to CSV and save to <output_file>.\n" ++
    "\n" ++
    "<filename>:\n" ++
    "  If only one filename is provided, the program will print the contents of that file.\n"

convertDividendToCsv :: String -> String -> IO ()
convertDividendToCsv inputFile outputFile = do
    let tagXMLa = [(Root, "ZLG00100"),
                             (Child, "ZLG00110"), (Child, "ZLG00120"), (Child, "ZLG00130")
                             , (Child, "ZLG00140"), (Child, "ZLG00150"), (Child, "ZLG00160")
                             , (Child, "ZLG00170"), (Child, "ZLG00180"), (Child, "ZLG00190")]
    let tagXMLb = [(Root, "ZLG00190"),
                             (Child, "gen:era"),
                             (Child, "gen:yy"), (Child, "gen:mm"), (Child, "gen:dd")]
    putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ " as CSV."
    -- Create the CSV header from the child tags
    let header = intercalate "," (map getTagString (filter (\t -> getTagType t == Child) tagXMLa))
    -- Try to convert the XML file to CSV
    resultA <- try (convertXmlFileToCsv inputFile tagXMLa) :: IO (Either SomeException [String])
    if isLeft resultA
        then putStrLn $ "Error processing XML file: " ++ show (fromLeft undefined resultA)
        else do
            let cvsData0 = fromRight [] resultA
            let cvsData1 = removeLastColumn cvsData0
            resultB <- try (convertXmlFileToCsv inputFile tagXMLb) :: IO (Either SomeException [String])
            if isLeft resultB
                then putStrLn $ "Error processing XML file: " ++ show (fromLeft undefined resultB)
                else do
                    let cvsData2 = fromRight [] resultB
                    -- Combine the two CSV data lists
                    let combinedCsvData = concatenateCsvRows cvsData1 cvsData2
                    -- Write the combined CSV data to the output file
                    let header2 = header ++ ",yy,mm,dd"
                    writeCsvDocument outputFile header2 combinedCsvData
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