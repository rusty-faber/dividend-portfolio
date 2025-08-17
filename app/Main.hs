module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)
import Lib

main :: IO ()
main = getArgs >>= executeCommand

executeCommand :: [String] -> IO ()
executeCommand ["-s", inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand ["-d", inputFile, outputFile] = convertInToOut convertDividendToCsv inputFile outputFile
executeCommand [inputFile, outputFile] = convertInToOut convertSampleXmlToCsv inputFile outputFile
executeCommand [filePath] = printFile printFileContents filePath
executeCommand _ = putStrLn usageText

usageText :: String
usageText =
  "Usage: program [options] <input_file> [output_file]\n\n" ++
  "Options:\n" ++
  "  -s <input_file> <output_file>\n" ++
  "      Convert sample XML data from <input_file> to CSV and save to <output_file>.\n\n" ++
  "  -d <input_file> <output_file>\n" ++
  "      Convert dividend data from <input_file> to CSV and save to <output_file>.\n\n" ++
  "  <input_file> <output_file>\n" ++
  "      (Default) Convert sample XML data from <input_file> to CSV and save to <output_file>.\n\n" ++
  "<filename>:\n" ++
  "  If only one filename is provided, the program will print the contents of that file.\n"

-- Wrapper over convertXmlFileToCsv that returns [] on error and logs
extractXmlData :: FilePath -> [Tag] -> IO (Either SomeException [String])
extractXmlData filePath tags = try $ convertXmlFileToCsv filePath tags

fetchXmlList :: FilePath -> [Tag] -> String -> IO [String]
fetchXmlList input tags label = do
  res <- extractXmlData input tags
  case res of
    Left err -> do
      putStrLn $ "Error processing XML (" ++ label ++ "): " ++ show err
      return []
    Right xs -> do
      -- putStrLn $ "=== " ++ label ++ " ==="
      -- mapM_ putStrLn xs
      return xs

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

-- Normalize C2 list: handle single comma-joined string, then pick 1st and 3rd elements
normalizeAndFilterC2 :: [String] -> [String]
normalizeAndFilterC2 d =
  let items = case d of
                [single] | ',' `elem` single -> splitOn "," single
                _ -> d
  in case items of
       (x:_:z:_) -> [x, z]
       [x]       -> [x]
       _         -> []

-- Append combined C1/C2/date row to combinedData
appendC1C2ToCombinedData :: FilePath -> [String] -> IO [String]
appendC1C2ToCombinedData inputFile combinedData = do
  let tagsXMLd = [(Root,"ZLB00000"), (Child,"gen:era"), (Child,"gen:yy"), (Child,"gen:mm"), (Child,"gen:dd")]
      tagsXMLc1 = [(Root,"ZLF00010"), (Child,"ZLF00020"), (Child,"ZLF00030"), (Child,"ZLF00040")]
      tagsXMLc2 = [(Root,"ZLF00050"), (Child,"ZLF00060"), (Child,"ZLF00080"), (Child,"ZLF00100")]

  westernDateData <- fetchXmlList inputFile tagsXMLd "structure D - date" >>= (return . convertDateStringsToWesternString)
  rawDataC1        <- fetchXmlList inputFile tagsXMLc1 "structure C1"
  rawDataC2        <- fetchXmlList inputFile tagsXMLc2 "structure C2"
  let filteredC2 = normalizeAndFilterC2 rawDataC2

  let combinedRow = intercalate "," $
        ["分離課税", "上場株式等に係る譲渡所得等の金額"]
        ++ filteredC2 ++ rawDataC1 ++ ["0"] ++ westernDateData

  return (combinedData ++ [combinedRow])

-- Main logic to process dividend data
processDividendData :: FilePath -> FilePath -> IO ()
processDividendData inputFile outputFile = do
  putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ " as CSV."

  let tagsXMLa = [(Root,"ZLG00100"), (Child,"ZLG00110"), (Child,"ZLG00120"), (Child,"ZLG00130"),
                  (Child,"ZLG00140"), (Child,"ZLG00150"), (Child,"ZLG00160"),
                  (Child,"ZLG00170"), (Child,"ZLG00180"), (Child,"ZLG00190")]
      tagsXMLd = [(Root,"ZLG00190"), (Child,"gen:era"), (Child,"gen:yy"), (Child,"gen:mm"), (Child,"gen:dd")]

  let headerZ = generateCsvHeader tagsXMLa
      header = convertHeaderZToHeader headerZ

  resultA <- extractXmlData inputFile tagsXMLa
  case resultA of
    Left err -> putStrLn $ "Error processing XML (structure A): " ++ show err
    Right rawDataA -> do
      let dividendDataRaw = removeLastColumn rawDataA
          dividendData = removeZenkakuSpace dividendDataRaw

      dateData <- fetchXmlList inputFile tagsXMLd "structure D - date"
      let westernDateData = convertDateStringsToWesternString dateData
          combinedData = concatenateCsvRows dividendData westernDateData

      combinedData' <- appendC1C2ToCombinedData inputFile combinedData

      writeCsvDocument outputFile header combinedData'
      putStrLn $ "Successfully converted " ++ inputFile ++ " to " ++ outputFile

convertDividendToCsv :: String -> String -> IO ()
convertDividendToCsv = processDividendData

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

parseEraDate :: String -> Maybe (Int, Int, Int, Int)
parseEraDate str = case map read (splitOn "," str) of
  [era, yy, mm, dd] -> Just (era, yy, mm, dd)
  [era, yy] -> Just (era, yy, 12, 31)
  [era] -> Just (era, 0, 12, 31)
  _ -> Nothing

eraToWesternString :: (Int, Int, Int, Int) -> Maybe String
eraToWesternString (era, yy, mm, dd) =
  case lookup era eraStartYears of
    Just startYear -> Just (show (startYear + yy) ++ "," ++ show mm ++ "," ++ show dd)
    Nothing        -> Nothing

convertDateStringsToWesternString :: [String] -> [String]
convertDateStringsToWesternString = mapMaybe convertSingleDate

convertSingleDate :: String -> Maybe String
convertSingleDate s = parseEraDate s >>= eraToWesternString

removeZenkakuSpace :: [String] -> [String]
removeZenkakuSpace = map (filter (/= '\x3000'))
