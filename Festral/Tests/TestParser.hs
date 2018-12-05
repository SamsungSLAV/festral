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
--
-- You can create own parser scripts for festral test. Such script __MUST__
-- get log with test result as its stdin and put parsed statistics to the
-- stdout.
--
-- Parsed data has format:
--
-- @
-- ######################################
-- Test name,test id, testcase name,result of preparing for test,result of
-- test,result of cleaning after test,spent time
-- ...
-- ######################################
-- @
--
-- Result of test can be TEST_PASS or TEST_FAIL.
--
-- Example of such output:
--
-- @
-- ###########################################################
-- Xtest,0,regression_1001,TEST_PASS,TEST_PASS,TEST_PASS,0.0
-- Xtest,1,regression_1002,TEST_PASS,TEST_PASS,TEST_PASS,0.0
-- Xtest,2,regression_1003,TEST_FAIL,TEST_FAIL,TEST_FAIL,0.0
-- Xtest,21,regression_1010.1,TEST_PASS,TEST_PASS,TEST_PASS,0.0
-- ###########################################################
-- @
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

import Festral.Files

-- |This data structure stores information needed by database for importing
-- tests results.
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
    { out   :: String   -- ^ Test output
    } deriving Show

fromFile :: FilePath -> IO TestParser
fromFile fname = do
    log <- safeReadFile fname
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

-- |Simple test results parser. If line contains \"OK\" it increment passed
-- tests number, otherwise increments only all tests count. It means that this
-- parser counts tests like one line = one test result.
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

-- |Parser for the output of the XTest - official test suit of OPTEE.
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
