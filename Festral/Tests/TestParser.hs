{-
 - Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
 -
 - Author: Uladzislau Harbuz <u.harbuz@samsung.com>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -      http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License
 -}

-- |Module which describes API for parsing and exporting tests results to the
-- special report format.
module Festral.Tests.TestParser (
    TestData (..),
    TestParser (..),
    writeReportFile,
    fromFile,
    parseDefault,
    parseXTest,
    fromWelesFiles
) where

import System.IO
import Data.List.Split
import Data.Char
import Data.List

-- |This data structure stores information needed by database for importing
-- tests results
data TestData = TestData
    { testGroup :: String   -- ^Group of the test, e.g. regression21015
    , testIdx   :: Int      -- ^Index of the test in the group
    , testName  :: String   -- ^Name of the test
    , preRes    :: String   -- ^Result of the preparing for test (TEST_PASS on success)
    , testRes   :: String   -- ^Result of the test (TEST_PASS on success)
    , postRes   :: String   -- ^Result of the post-test operations (TEST_PASS on success)
    , testTime  :: String   -- ^Time taken by test execution
    } deriving Show

data TestParser = TestParser
    { out   :: String
    } deriving Show

fromFile :: FilePath -> IO TestParser
fromFile fname = do
    log <- readFile fname
    return $ TestParser log

-- |Get parser for specified Weles file. Arguments are [(Filename, content)] -
-- list of all weles files; second argument is target filename.
fromWelesFiles :: [(String, String)] -> String -> IO TestParser
fromWelesFiles [] _ = return $ TestParser ""
fromWelesFiles files logname = do
    let x = filter (\(n,c) -> logname `isInfixOf` n) files
    return $ getParser x
    where
        getParser ((fname, content):_) = TestParser content
        getParser _ = TestParser ""

parseDefault :: TestParser -> [TestData]
parseDefault parser = zipWith (\[name,res] i ->
                        TestData
                            "Default test"
                            i
                            name
                            (tr res)
                            (tr res)
                            (tr res)
                            "0.5") res [1 ..]
    where
        res = map (dropWhile isSpace) <$> (filter ((== 2) . length) $
                splitOn "..." <$> splitOn "\n" (out parser))
        tr "OK" = "TEST_PASS"
        tr _ = "TEST_FAIL"

parseXTest :: TestParser -> [TestData]
parseXTest parser = zipWith (\[name,res] i ->
                        TestData "Xtest" i name res res res "0.5") res [1 ..]
    where
        res = map (\x-> [head x, if "OK" `elem` x
                                    then "TEST_PASS"
                                    else "TEST_FAIL"]) $
            (filter (not . (==) "") <$>) <$>
            filter (\x-> "OK" `elem` x || "FAILED" `elem` x) $
            splitOn " " <$> splitOn "\n" (out parser)

-- |Write results of the test to the special-formatted (whitch is understood
-- by database importing scripts) report file with given path
writeReportFile :: [TestData] -> FilePath -> IO ()
writeReportFile resLst fname = do
    let rows = processRow <$> resLst
    let body = foldl (\y str -> str ++ "\n" ++ y) "" rows
    let all = "##################################################\n"
             ++ body ++ "#####################################################\
             \############"
    writeFile fname all

    where
    processRow x = init $ foldl (\y str -> str ++ "," ++ y) "" $
                    reverse [a, show b, c,d,e,f,g]
        where
            (TestData a b c d e f g) = x
