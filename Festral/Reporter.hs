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

{-# LANGUAGE DeriveGeneric #-}

-- | Module for generating reports from build and test results.
module Festral.Reporter (
    reportHTML,
    formatTextReport,
    getTestResults,
    testReport,
    aging,
    csv2TestData,
    report2csv,
    mapT2,
    AgingResult(..)
) where

import System.IO
import System.Directory
import Data.List.Split
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Aeson
import Data.List
import Data.Time.LocalTime
import Data.List.Utils
import Text.Read
import Data.Maybe
import Control.Exception
import qualified Data.HashMap.Strict as Map
import GHC.Generics

import Festral.Template
import Festral.Internal.Files
import Festral.Meta hiding (repoName, testName)
import qualified Festral.Meta as M (repoName, testName)
import Festral.Config
import qualified Festral.Tests.Data as T
import Festral.Tests.Data (TestData)

data BuildSummary = BuildSummary
    { repoName      :: String
    , branchName    :: String
    , buildStatus   :: String
    , logLink       :: String
    }

data TestSummary = TestSummary
    { bSummary      :: BuildSummary
    , testName      :: String
    , targetName    :: String
    , testResult    :: String
    }

-- |Representation of aging test result. It assumes that same set of tests was
-- ran multiple times and 'TestData' contains  lots of records wits same name
-- of test and mostly same results, where in some iterations of test deviations
-- from first running can appear.
--
-- @since 1.4.0
data AgingResult = AgingResult
    { results       :: TestData
    -- ^ Results of the first iteration of tests.
    , deviations    :: [Int]
    -- ^ Numbers of test iterations where deviations of pass appeared.
    , nIterations   :: Int
    -- ^ Total number of performed iterations of test.
    } deriving (Show, Generic)

instance ToJSON AgingResult

defaultHTML time =
              "<!DOCTYPE html>\n"
           ++ "<html>\n"
           ++ "<head>\n"
           ++ "<title>Test report</title>\n"
           ++ "<style>\n"
           ++ "    table,td,tr, th {\n"
           ++ "        border: 1px solid black;\n"
           ++ "    }\n"
           ++ "</style>\n"
           ++ "</head>\n"
           ++ "<body>\n"
           ++ "    <h2>Test summary report for "++ time ++ "</h2>\n"
           ++ "    <h2>Build summary:</h2>\n"
           ++ "    ##TEMPLATE_BUILD_TABLE buildTable##\n"
           ++ "    <h2>Test summary:</h2>\n"
           ++ "    ##TEMPLATE_TEST_TABLE testTable##\n"
           ++ "</body>\n"
           ++ "</html>\n"

-- |Get parsed result of test specified by id in CSV format with #(hash)
-- characters line at the begin and at the end of results. Manually it is just
-- parsed by "Festral.Tests.TestParser" raw test output. If test was performed
-- successfully returns parsed result, otherwise returns 'Nothing'.
--
-- @since 1.4.0
getTestResults :: AppConfig -> String -> IO (Maybe String)
getTestResults appConf id = do
    handle badFile $ Just <$> (readFile $ reportFilePath appConf id)
    where
        badFile :: SomeException -> IO (Maybe String)
        badFile _ = return Nothing

-- |Get results of test specified by its id.
--
-- @since 1.4.0
testReport :: AppConfig -> String -> IO [TestData]
testReport a id = do
    report <- getTestResults a id
    return $ maybe [] (csv2TestData . report2csv) report

-- |Count deviations and merge results of aging tests. It assumes that one
-- test set was ran multiple times so passed 'TestData' contains a lot of
-- repeated tests, where some of these repeated tests can has different from
-- first iteration results (named deviations).
--
-- Returned data contains only one first iteration result of every performed
-- test and information about its deviations and total iterations performed.
--
-- @since 1.4.0
aging :: [TestData] -> [AgingResult]
aging tests = Map.elems $ foldl' acc Map.empty tests
    where
        acc res x = Map.insertWith f (T.testName x) (AgingResult x [] 1) res
        f :: AgingResult -> AgingResult -> AgingResult
        f new old = old
            { deviations = (incIfDiff old new)
            , nIterations = (nIterations old) + 1
            }
        incIfDiff :: AgingResult -> AgingResult -> [Int]
        incIfDiff old new = if g old == g new
                                then (deviations old)
                                else (deviations old) ++ [nIterations old]
        g x = [T.prepareRes, T.testRes, T.testCleanRes] <*> [results x]

-- |Generate HTML report file with results given by second parameter.
reportHTML :: AppConfig -- ^ Program configuration with build and test log directories specified.
           -> String    -- ^ Template HTML file to be filled with report data.
                        -- If it is empty, generate default simpliest HTML page.
           -> [String]  -- ^ List of names of builds and tests in format
                        -- returned by 'build' and 'performForallNewBuilds'
                        -- commands.
           -> IO String -- ^ Generated HTML report.
reportHTML config "" dirs = show <$> getZonedTime >>=
    (\time -> reportHTML config (defaultHTML time) dirs)
reportHTML config src dirs = generateFromTemplate src (templateHTML config dirs)

-- |Make text report when every line has a format like passed in first argument.
-- Format string has special characters:
--
-- +---------+-----------------------+-----------------------------------------+
-- |Specifier|    Output             |    Example                              |
-- +=========+=======================+=========================================+
-- | %b      | board                 | armv7l                                  |
-- +---------+-----------------------+-----------------------------------------+
-- | %t      | build type            | debug                                   |
-- +---------+-----------------------+-----------------------------------------+
-- | %c      | commit name           | 60fbeee6f89e2a61417033a854b3d2fdfc9f1a58|
-- +---------+-----------------------+-----------------------------------------+
-- | %T      | build time            | 20181009112502                          |
-- +---------+-----------------------+-----------------------------------------+
-- | %C      | toolchain             | GBS                                     |
-- +---------+-----------------------+-----------------------------------------+
-- | %u      | builder username      | test.user                               |
-- +---------+-----------------------+-----------------------------------------+
-- | %s      | build status          | SUCCEED                                 |
-- +---------+-----------------------+-----------------------------------------+
-- | %h      | build hash            | 60fbeee6f89e2a61417033a854b3d2fdfc9f1a58|
-- +---------+-----------------------+-----------------------------------------+
-- | %o      | build output directory| /GBS-ROOT/local/tizen_arm/armv7l/RPMS   |
-- +---------+-----------------------+-----------------------------------------+
-- | %r      | name of the repository| some-test                               |
-- +---------+-----------------------+-----------------------------------------+
-- | %B      | branch name           | master                                  |
-- +---------+-----------------------+-----------------------------------------+
-- | %l      | tester login          | tester.login                            |
-- +---------+-----------------------+-----------------------------------------+
-- | %L      | tester name           | Kowalski                                |
-- +---------+-----------------------+-----------------------------------------+
-- | %e      | test time             | 20181009112502                          |
-- +---------+-----------------------+-----------------------------------------+
-- | %n      | test name             | SOME TEST                               |
-- +---------+-----------------------+-----------------------------------------+
-- | %S      | test status           | COMPLETE                                |
-- +---------+-----------------------+-----------------------------------------+
-- | %d      | test device           | rpi3                                    |
-- +---------+-----------------------+-----------------------------------------+
-- | %R      | pass rating passed/all| 55/210                                  |
-- +---------+-----------------------+-----------------------------------------+
-- | %A      | aging test results:   |                                         |
-- |         | deviations from first |                                         |
-- |         | iteration/number of   |                                         |
-- |         | all iterations        | 4/2.98                                  |
-- +---------+-----------------------+-----------------------------------------+
-- |%%       | insert % character    | %                                       |
-- +---------+-----------------------+-----------------------------------------+
--
-- Default format is \"%r[%B] Build: %s Test: %R\".
formatTextReport :: AppConfig   -- ^ Program configuration with buildLog and
                                -- testLog configured.
                 -> String      -- ^ Format string.
                 -> [String]    -- ^ List of names of builds and tests in format
                                -- returned by 'build' and
                                -- 'performForallNewBuilds'.
                 -> IO [String] -- ^ Generated textual report.
formatTextReport config format dirs = do
    metas <- mapM (metaByName config) dirs
    let tests = filter (\ (n,m) -> isMeta m) metas
    mapM f tests
    where
        f (n,m) = do
            let str = foldl (\ s (f,o) -> replace f (o m) s) format formats
            (r, a) <- if isTest m
                then do
                    t <- (testSummary config) n
                    report <- testReport config n
                    let a = aging report
                    let (agingDeviations, agingTotalIter)
                            = foldl'
                            (\(d,i) x -> (d + (length $ deviations x),
                                          i + (nIterations x)))
                            (0,0) a
                    let agingRes = if testStatus m == "COMPLETE"
                                    then show agingDeviations
                                        ++ "/"
                                        ++ show (fromIntegral agingTotalIter
                                        / fromIntegral (length a))
                                    else testStatus m
                    return $ (testResult t, agingRes)
                else return ("", "")
            return $ foldl
                (\ str (key, f) -> replace key f str)
                str
                (zip ["%R", "%A"] [r, a])

testOnly f m@MetaTest{} = f m
testOnly _ _ = ""

formats =
    [("%%", (\ _ -> "%"))
    ,("%b", ($>>) board)
    ,("%t", ($>>) buildType)
    ,("%c", ($>>) commit)
    ,("%T", ($>>) buildTime)
    ,("%C", ($>>) toolchain)
    ,("%u", ($>>) builder)
    ,("%s", ($>>) status)
    ,("%h", ($>>) hash)
    ,("%o", ($>>) outDir)
    ,("%r", ($>>) M.repoName)
    ,("%B", ($>>) branch)
    ,("%l", testOnly tester)
    ,("%L", testOnly testerName)
    ,("%e", testOnly testTime)
    ,("%n", testOnly M.testName)
    ,("%S", testOnly testStatus)
    ,("%d", testOnly testDevice)
    ]

-- |Converts string in CSV format into the 'TestData'.
--
-- @since 1.4.0
csv2TestData :: String -> [TestData]
csv2TestData = catMaybes . map readMaybe . lines

-- |Converts raw report format into the CSV format. Actually get only part of
-- string located between more than 20 #(sharp) characters.
--
-- @since 1.4.0
report2csv :: String -> String
report2csv = unlines . parseTestRes
           . splitWhen (isInfixOf $ replicate 20 '#') . lines

makeBuildRow :: BuildSummary -> String
makeBuildRow b
    = "<tr><td>" ++ repoName b ++ "</td><td>"
    ++ branchName b ++ "</td><td>"
    ++ buildStatus b ++ "</td><td><a href=\""
    ++ logLink b ++ "\">log</a></td></tr>"

makeTestRow :: TestSummary -> String
makeTestRow t
    = "<tr><td>" ++ repoName b ++ "</td><td>" ++ branchName b
    ++ "</td><td>" ++ targetName t ++ "</td><td>" ++ testName t ++ "</td><td>"
    ++ testResult t ++ "</td><td><a href=\""
    ++ logLink b ++ "\">log</a></td></tr>"
    where b = bSummary t

-- |Gets name of the build (sha1_time) and returns its build data.
buildSummary :: AppConfig -> String -> IO BuildSummary
buildSummary config dir = do
    meta <- fromMetaFile $ buildLogDir config ++ "/" ++ dir ++ "/meta.txt"
    let link = "http://" ++ webPageIP config ++ ":" ++ show (webPagePort config)
            ++ "/getlog?type=build&hash="
            ++ hash $>> meta ++ "&time=" ++ buildTime $>> meta
    return $ BuildSummary
        (M.repoName $>> meta)
        (branch $>> meta)
        (status $>> meta)
        link

-- |Gets name of the test result (sha1_time) and returns its build data as
-- (repository name, branch name, target device, test name,
-- passed tests/ all tests, log link)
testSummary :: AppConfig -> String -> IO TestSummary
testSummary config dir = do
    meta <- fromMetaFile $ testLogDir config ++ "/" ++ dir ++ "/meta.txt"

    report <- testReport config dir
    let pass = mapT2 length (filter isPassed report, report)
    let link = "http://" ++ webPageIP config ++ ":" ++ show (webPagePort config)
             ++ "/getlog?type=test&hash=" ++ hash $>> meta
             ++ "&time=" ++ testTime meta
    return $ TestSummary
        (BuildSummary
            (M.repoName $>> meta)
            (branch $>> meta)
            (status $>> meta)
            link )
        (M.testName meta)
        (testDevice meta)
        (percents pass $ testStatus meta)

percents :: (Int, Int) -> String -> String
percents (0,0) "COMPLETE" = "NO RESULTS"
percents (0,0) status = status
percents x@(pass, all) "SEGFAULT" = show pass ++ "/" ++ show all ++ "(SEGFAULT)"
percents x@(pass, all) _ = show pass ++ "/" ++ show all

isPassed :: TestData -> Bool
isPassed x = T.testRes x == T.TEST_PASS

-- |Equivalent of the 'map' function for mono type tuples. This function is
-- useful for mapping throw 'partition' output and similar functions.
--
-- @since 1.4.0
mapT2 f (a,b) = (f a, f b)

parseTestRes :: [[String]] -> [String]
parseTestRes (_:x:_) = x
parseTestRes _ = []

templateHTML :: AppConfig -> [String] -> TemplateType -> IO String
templateHTML config dirs (BuildTable id) = do
    metas <- mapM (metaByName config) dirs
    let builds = fst <$> filter (\ (n,m) -> isBuild m) metas
    buildSummaries <- sequence $ map (buildSummary config) builds
    let rows = concat $ map makeBuildRow buildSummaries
    return $  "    <table id=\"" ++ id ++ "\">\n"
           ++ "        <thead><tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Build result\
           \</th><th>Log file</th>\n"
           ++ "        </tr></thead>\n"
           ++ "        <tbody>" ++ rows ++ "</tbody>\n"
           ++ "    </table>\n"

templateHTML config dirs (TestTable id) = do
    metas <- mapM (metaByName config) dirs
    let tests = fst <$> filter (\ (n,m) -> isTest m) metas
    testSummaries <- sequence $ map (testSummary config) tests
    let rows = concat $ map makeTestRow testSummaries
    return $  "    <table id=\"" ++ id ++ "\">\n"
           ++ "        <thead><tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Device</th>\
           \<th>Test name</th><th>Test result</th><th>Log file</th>\n"
           ++ "        </tr></thead>\n"
           ++ "        <tbody>" ++ rows ++ "</tbody>\n"
           ++ "    </table>\n"

templateHTML _ _ _ = return ""

metaByName config name = do
    let build = buildLogDir config ++ "/" ++ name ++ "/meta.txt"
    let test = testLogDir config ++ "/" ++ name ++ "/meta.txt"
    tMeta <- fromMetaFile test
    meta <- if isTest tMeta
                then return tMeta
                else fromMetaFile build
    return (name, meta)
