-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"

main :: IO ()
main = do
    let xmlContent = "<Stock><Symbol>AAPL</Symbol><Name>Apple Inc.</Name><Price>170.50</Price></Stock>"
    let tagsToExtract = [(Root, "Stock"), (Child, "Symbol"), (Child, "Name"), (Child, "Price")]
    result <- runX (readString [withValidate no] xmlContent >>> processXmlToCsv tagsToExtract)
    print result -- Output: ["AAPL,Apple Inc.,170.50"]

    let xmlContent2 = "<Product><ID>123</ID><Description>Awesome Gadget</Description></Product>"
    let tagsToExtract2 = [(Root, "Product"), (Child, "ID"), (Child, "Description")]
    result2 <- runX (readString [withValidate no] xmlContent2 >>> processXmlToCsv tagsToExtract2)
    print result2 -- Output: ["123,Awesome Gadget"]
