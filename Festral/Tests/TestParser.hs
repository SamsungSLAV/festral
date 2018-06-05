module Festral.Tests.TestParser (
    TestData (..),
    writeReportFile,
    TestParser,
    fromFile,
    parse
) where

class TestParser a where
    fromFile :: FilePath -> IO a
    parse :: a -> [TestData]

data TestData = TestData
    { testGroup :: String
    , testIdx   :: Int
    , testName  :: String
    , preRes    :: String
    , testRes   :: String
    , postRes   :: String
    , testTime  :: String
    } deriving Show

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
