{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.XML.HXT.Core
import Lib

assert :: Bool -> String -> String -> IO ()
assert test passMsg failMsg = if test
    then putStrLn passMsg
    else putStrLn failMsg

main :: IO ()
main = do
    let xmlContent1 = "<Stock><Symbol>AAPL</Symbol><Name>Apple Inc.</Name><Price>170.50</Price></Stock>"
    let tagsToExtract1 = [(Root, "Stock"), (Child, "Symbol"), (Child, "Name"), (Child, "Price")]
    result1 <- runX (readString [withValidate no] xmlContent1 >>> processXmlToCsv tagsToExtract1)
    -- print result1 -- Output: ["AAPL,Apple Inc.,170.50"]

    let xmlContent2 = "<Product><ID>123</ID><Description>Awesome Gadget</Description></Product>"
    let tagsToExtract2 = [(Root, "Product"), (Child, "ID"), (Child, "Description")]
    result2 <- runX (readString [withValidate no] xmlContent2 >>> processXmlToCsv tagsToExtract2)
    -- print result2 -- Output: ["123,Awesome Gadget"]
    
    let xmlContent3 = "<Data><Item><Value1>One</Value1><Value2>Two</Value2></Item></Data>"
    let tagsToExtract3 = [(Root, "Data"), (Child, "Item"), (Child, "Value1"), (Child, "Value2")]
    result3 <- runX (readString [withValidate no] xmlContent3 >>> processXmlToCsv tagsToExtract3)
    -- print result3 -- Output: ["One,Two"]

    let xmlContent4 = "<Info><Name>John Doe</Name><Age>30</Age></Info>"
    let tagsToExtract4 = [(Root, "Info"), (Child, "Name"), (Child, "Age")]
    result4 <- runX (readString [withValidate no] xmlContent4 >>> processXmlToCsv tagsToExtract4)
    -- print result4 -- Output: ["John Doe,30"]

    assert (result1 == ["AAPL,Apple Inc.,170.50"]) "Test 1 passed" "Test 1 failed"
    assert (result2 == ["123,Awesome Gadget"]) "Test 2 passed" "Test 2 failed"
    assert (result3 == ["One,Two"]) "Test 3 passed" "Test 3 failed" 
    assert (result4 == ["John Doe,30"]) "Test 4 passed" "Test 4 failed"
    putStrLn "All tests completed."
    
    return ()