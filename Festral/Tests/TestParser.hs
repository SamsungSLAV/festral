-- |Module which describes API for parsing and exporting tests results to the special report format.
module Festral.Tests.TestParser (
    TestData (..),
    writeReportFile,
    TestParser,
    fromFile,
    parse,
    fromWelesFiles
) where

-- |Represents everything that can parse tests
class TestParser a where
    -- |Make parser from file with tests output
    fromFile :: FilePath -> IO a
    -- |Make parser from output of the Weles (got by 'runTest' from "Test" module)
    fromWelesFiles :: [(String, String)] -> IO a
    -- |Parse tests output to the 'TestData' records
    parse :: a -> [TestData]

-- |This data structure stores information needed by database for importing tests results
data TestData = TestData
    { testGroup :: String   -- ^Group of the test, e.g. TCT_TEST_TA
    , testIdx   :: Int      -- ^Index of the test in the group
    , testName  :: String   -- ^Name of the test
    , preRes    :: String   -- ^Result of the preparing for test (TEST_PASS on success)
    , testRes   :: String   -- ^Result of the test (TEST_PASS on success)
    , postRes   :: String   -- ^Result of the post-test operations (TEST_PASS on success)
    , testTime  :: String   -- ^Time taken by test execution
    } deriving Show

-- |Write results of the test to the special-formatted (whitch is understood by database importing scripts) report file with given path
writeReportFile :: [TestData] -> FilePath -> IO ()
writeReportFile resLst fname = do
    let rows = processRow <$> resLst
    let body = foldl (\y str -> str ++ "\n" ++ y) "" rows
    let all = "##################################################\n" ++ body ++ "#################################################################"
    writeFile fname all
    
    where
    processRow x = init $ foldl (\y str -> str ++ "," ++ y) "" $ reverse [a, show b, c,d,e,f,g]
        where
            (TestData a b c d e f g) = x
