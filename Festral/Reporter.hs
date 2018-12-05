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

-- | Module for generating reports from build and test results.
module Festral.Reporter (
    reportHTML,
    formatTextReport
) where

import           System.IO
import           System.Directory
import           Data.List.Split
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson
import           Data.List
import           Data.Time.LocalTime
import           Data.List.Utils

import           Festral.Template
import           Festral.Files
import           Festral.Meta
import           Festral.Config

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

-- |Generate HTML report file with results given by second parameter
reportHTML :: String    -- ^ Template HTML file to be filled with report data.
                        -- If it is empty, generate default simpliest HTML page.
           -> [String]  -- ^ List of names of builds and tests in format
                        -- returned by 'build' and 'performForallNewBuilds'
                        -- commands.
           -> IO String -- ^ Generated HTML report.
reportHTML "" dirs = show <$> getZonedTime >>=
    (\time -> reportHTML (defaultHTML time) dirs)
reportHTML src dirs = generateFromTemplate src (templateHTML dirs)

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
-- |%%       | insert % character    | %                                       |
-- +---------+-----------------------+-----------------------------------------+
--
-- Default format is \"%r[%B] Build: %s Test: %R\".
formatTextReport :: String      -- ^ Format string.
                 -> [String]    -- ^ List of names of builds and tests in format
                                --  returned by 'build' and
                                --  'performForallNewBuilds'
                 -> IO [String] -- ^ Generated textual report.
formatTextReport format dirs = do
    metas <- mapM metaByName dirs
    let tests = filter (\ (n,m) -> isMeta m) metas
    mapM f tests
    where
        f (n,m) = do
            let str = foldl (\ s (f,o) -> replace f (o m) s) format formats
            r <- if isTest m
                then do
                    (_,_,_,_,rating,_) <- testSummary n
                    return rating
                else return ""
            return $ replace "%R" r str

testOnly f m@MetaTest{} = f m
testOnly _ _ = ""

formats =
    [("%b", ($>>) board)
    ,("%t", ($>>) buildType)
    ,("%c", ($>>) commit)
    ,("%T", ($>>) buildTime)
    ,("%C", ($>>) toolchain)
    ,("%u", ($>>) builder)
    ,("%s", ($>>) status)
    ,("%h", ($>>) hash)
    ,("%o", ($>>) outDir)
    ,("%r", ($>>) repoName)
    ,("%B", ($>>) branch)
    ,("%l", testOnly tester)
    ,("%L", testOnly testerName)
    ,("%e", testOnly testTime)
    ,("%n", testOnly testName)
    ,("%S", testOnly testStatus)
    ,("%d", testOnly testDevice)
    ,("%%", (\ _ -> "%"))
    ]

makeBuildRow :: (String, String, String, String) -> String
makeBuildRow (repo, branch, status, link)
    = "<tr><td>" ++ repo ++ "</td><td>"
    ++ branch ++ "</td><td "++ color status ++ ">"
    ++ status ++ "</td><td><a href=\"" ++ link ++ "\">log</a></td></tr>"

makeTestRow :: (String, String, String, String, String, String) -> String
makeTestRow (repo, branch, device, name, status, link)
    = "<tr><td>" ++ repo ++ "</td><td>" ++ branch
    ++ "</td><td>" ++ device ++ "</td><td>" ++ name ++ "</td><td "
    ++ color status ++">" ++ status ++ "</td><td><a href=\""
    ++ link ++ "\">log</a></td></tr>"

color "SUCCEED" = "style=\"color:green;\""
color "FAILED" = "style=\"color:red;\""
color _ = ""

-- |Gets name of the build (sha1_time) and returns its build
-- data as (repository name, branch name, build status)
buildSummary :: String -> IO (String, String, String, String)
buildSummary dir = do
    config <- getAppConfig
    meta <- fromMetaFile $ buildLogDir config ++ "/" ++ dir ++ "/meta.txt"
    let link = "http://" ++ webPageIP config ++ ":" ++ show (webPagePort config)
            ++ "/getlog?type=build&hash="
            ++ hash $>> meta ++ "&time=" ++ buildTime $>> meta
    return (repoName $>> meta, branch $>> meta, status $>> meta, link)

-- |Gets name of the test result (sha1_time) and returns its build data as
-- (repository name, branch name, test name, passed tests/ all tests, log link)
testSummary :: String -> IO (String, String, String, String, String, String)
testSummary dir = do
    config <- getAppConfig
    meta <- fromMetaFile $ testLogDir config ++ "/" ++ dir ++ "/meta.txt"

    let reportPath = testLogDir config ++ "/" ++ dir ++ "/report.txt"
    report <- safeReadFile reportPath

    let tests = parseTestRes $ splitWhen (isInfixOf "###############") $
            splitOn "\n" report
    let pass= foldl (\ (x,y) b -> (if b then x+1 else x, y+1)) (0,0) $
            processReport <$> splitOn "," <$> tests
    let link = "http://" ++ webPageIP config ++ ":" ++ show (webPagePort config)
             ++ "/getlog?type=test&hash=" ++ hash $>> meta
             ++ "&time=" ++ testTime meta
    return (repoName $>> meta, branch $>> meta, testDevice meta, testName meta,
            percents pass (testStatus meta), link)

percents :: (Int, Int) -> String -> String
percents (0,0) "COMPLETE" = "NO RESULTS"
percents (0,0) status = status
percents x@(pass, all) "SEGFAULT" = show pass ++ "/" ++ show all ++ "(SEGFAULT)"
percents x@(pass, all) _ = show pass ++ "/" ++ show all

col x = "rgb(" ++ show (round (maxCol - passCol x)) ++ ","
    ++ show (round (passCol x)) ++ ",0)"
passCol (pass, all) = (maxCol/fromIntegral(all)) * fromIntegral(pass)

maxCol = 150

processReport :: [String] -> Bool
processReport (_:_:_:_:"TEST_PASS":_) = True
processReport _ = False

parseTestRes :: [[String]] -> [String]
parseTestRes (_:x:_) = x
parseTestRes _ = []

templateHTML :: [String] -> TemplateType -> IO String
templateHTML dirs (BuildTable id) = do
    metas <- mapM metaByName dirs
    let builds = fst <$> filter (\ (n,m) -> isBuild m) metas
    buildSummaries <- sequence $ map buildSummary builds
    let rows = concat $ map makeBuildRow buildSummaries
    return $  "    <table id=\"" ++ id ++ "\">\n"
           ++ "        <thead><tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Build result\
           \</th><th>Log file</th>\n"
           ++ "        </tr></thead>\n"
           ++ "        <tbody>" ++ rows ++ "</tbody>\n"
           ++ "    </table>\n"

templateHTML dirs (TestTable id) = do
    metas <- mapM metaByName dirs
    let tests = fst <$> filter (\ (n,m) -> isTest m) metas
    testSummaries <- sequence $ map testSummary tests
    let rows = concat $ map makeTestRow testSummaries
    return $  "    <table id=\"" ++ id ++ "\">\n"
           ++ "        <thead><tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Device</th>\
           \<th>Test name</th><th>Test result</th><th>Log file</th>\n"
           ++ "        </tr></thead>\n"
           ++ "        <tbody>" ++ rows ++ "</tbody>\n"
           ++ "    </table>\n"

templateHTML _ _ = return ""

metaByName name = do
    config <- getAppConfig
    let build = buildLogDir config ++ "/" ++ name ++ "/meta.txt"
    let test = testLogDir config ++ "/" ++ name ++ "/meta.txt"
    tMeta <- fromMetaFile test
    meta <- if isTest tMeta
                then return tMeta
                else fromMetaFile build
    return (name, meta)
