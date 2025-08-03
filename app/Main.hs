module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
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

-- Function to extract text content from XML file based on provided tags
extractXmlData :: FilePath -> [Tag] -> IO (Either SomeException [String])
extractXmlData filePath tags = try $ convertXmlFileToCsv filePath tags

-- Function to generate CSV header from a list of tags
generateCsvHeader :: [Tag] -> String
generateCsvHeader tags = intercalate "," (map getTagString (filter (\t -> getTagType t == Child) tags))

convertHeaderZToHeader :: String -> String
convertHeaderZToHeader headerZ =
  let mapping = [ ("ZLG00110", "Category")
                , ("ZLG00120", "Company")
                , ("ZLG00130", "Shares")
                , ("ZLG00140", "TaxableGain")
                , ("ZLG00150", "WithholdingTax")
                , ("ZLG00160", "LocalTax")
                , ("ZLG00170", "taxA")
                , ("ZLG00180", "taxB")
                , ("ZLG00190", "Year,Month,Day")
                ]
      convertTag tag = maybe tag id (lookup tag mapping)
  in intercalate "," $ map convertTag (splitOn "," headerZ)

-- Main logic to process dividend data
processDividendData :: FilePath -> FilePath -> IO ()
processDividendData inputFile outputFile = do
  putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ " as CSV."

  -- Definition of XML structure A for dividend information
  let tagsXMLa = [(Root, "ZLG00100"),
                  (Child, "ZLG00110"), (Child, "ZLG00120"), (Child, "ZLG00130"),
                  (Child, "ZLG00140"), (Child, "ZLG00150"), (Child, "ZLG00160"),
                  (Child, "ZLG00170"), (Child, "ZLG00180"), (Child, "ZLG00190")]

  -- Definition of XML structure B for date information
  let tagsXMLb = [(Root,"ZLG00190"),
                  (Child,"gen:era"), (Child,"gen:yy"), (Child,"gen:mm"), (Child,"gen:dd")]

  -- Generate CSV header for structure A
  let headerZ = generateCsvHeader tagsXMLa
  let header = convertHeaderZToHeader headerZ

  -- Extract data using XML structure A
  resultA <- extractXmlData inputFile tagsXMLa
  case resultA of
    Left err -> putStrLn $ "Error processing XML (structure A): " ++ show err
    Right rawDataA -> do
      -- Remove the last column from the extracted dividend data
      let dividendDataRaw = removeLastColumn rawDataA
      -- Remove Japanese characters and Zenkaku spaces from the dividend data
      let dividendData = removeZenkakuSpace dividendDataRaw

      -- Extract date data using XML structure B
      resultB <- extractXmlData inputFile tagsXMLb
      case resultB of
        Left err -> putStrLn $ "Error processing XML (structure B - date): " ++ show err
        Right dateData -> do
          -- Convert date data to Western years
          let westernDateData = convertDateStringsToWesternString dateData
          -- Combine the extracted dividend data and date data
          let combinedData = concatenateCsvRows dividendData westernDateData
          -- Write the combined CSV data to the output file
          writeCsvDocument outputFile header combinedData
          putStrLn $ "Successfully converted " ++ inputFile ++ " to " ++ outputFile

-- Top-level function to convert dividend data from XML to CSV
convertDividendToCsv :: String -> String -> IO ()
convertDividendToCsv inputFile outputFile = processDividendData inputFile outputFile

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

-- Maps Japanese era codes to their corresponding Western start years.
-- Era codes: 1: Meiji, 2: Taisho, 3: Showa, 4: Heisei, 5: Reiwa
eraStartYears :: [(Int, Int)]
eraStartYears =
  [ (1, 1867)  -- Meiji
  , (2, 1911)  -- Taisho
  , (3, 1925)  -- Showa
  , (4, 1988)  -- Heisei
  , (5, 2018)  -- Reiwa
  ]

-- A function to parse a string in the format "era,yy,mm,dd" into a date tuple (era, year, month, day).
-- Returns Nothing if parsing fails.
parseEraDate :: String -> Maybe (Int, Int, Int, Int)
parseEraDate str = case map read (splitOn "," str) of
  [era, yy, mm, dd] -> Just (era, yy, mm, dd)
  _                 -> Nothing

-- A function to convert a Japanese era date (era, year, month, day) to a Western date string "yyyy,mm,dd".
-- Returns Nothing if the era code is not recognized.
eraToWesternString :: (Int, Int, Int, Int) -> Maybe String
eraToWesternString (era, yy, mm, dd) =
  case lookup era eraStartYears of
    Just startYear -> Just (show (startYear + yy) ++ "," ++ show mm ++ "," ++ show dd)
    Nothing        -> Nothing

-- The main function to convert a list of date strings to a list of Western date strings.
-- Fails to convert an entry, that entry is omitted from the result.
convertDateStringsToWesternString :: [String] -> [String]
convertDateStringsToWesternString = mapMaybe convertSingleDate

-- A helper function that combines parsing and conversion for a single date string.
convertSingleDate :: String -> Maybe String
convertSingleDate s = parseEraDate s >>= eraToWesternString

--  Function to remove Zenkaku spaces (0x3000) from a list of strings.
removeZenkakuSpace :: [String] -> [String]
removeZenkakuSpace = map (filter (/= '\x3000'))
