module Festral.Builder.Reporter (
    reportHTML
) where

import System.IO
import System.Directory
import Data.List.Split
import Festral.Files
import Festral.Builder.Meta
import Festral.Config
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.List
import Data.Time.LocalTime

html time buildTable testTable =
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
           ++ "    <h2>Secos CI summary report for "++ time ++ "</h2>\n"
           ++ "    <h2>Build summary:</h2>\n"
           ++ "    <table>\n"
           ++ "        <tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Build result</th><th>Log file</th>\n"
           ++ "        </tr>\n"
           ++ "        " ++ buildTable ++ "\n"
           ++ "    </table>\n"
           ++ "    <h2>Test summary:</h2>\n"
           ++ "    <table>\n"
           ++ "        <tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Test name</th><th>Test result</th><th>Log file</th>\n"
           ++ "        </tr>\n"
           ++ "        " ++ testTable ++ "\n"
           ++ "    </table>\n"
           ++ "</body>\n"
           ++ "</html>\n"

-- |Generate HTML report file with results of the latest builds and tests
reportHTML :: IO String
reportHTML = do
    buildsFile <- freshBuilds
    builds <- readFile buildsFile
    buildSummaries <- sequence $ map buildSummary $ filter (not . (== "")) $ splitOn "\n" builds
    let rows = concat $ map makeBuildRow buildSummaries

    testsFile <- freshTests
    tests <- readFile testsFile
    testSummaries <- sequence $ map testSummary $ filter (not . (== "")) $ splitOn "\n" tests
    let testRows = concat $ map makeTestRow testSummaries

    time <- show <$> getZonedTime
    return $ html time rows testRows

makeBuildRow :: (String, String, String, String) -> String
makeBuildRow (repo, branch, status, link) = "<tr><td>" ++ repo ++ "</td><td>" ++ branch ++ "</td><td "++ color status ++">"
    ++ status ++ "</td><td><a href=\"" ++ link ++ "\">log</a></td></tr>"

makeTestRow :: (String, String, String, String, String) -> String
makeTestRow (repo, branch, name, status, link) = "<tr><td>" ++ repo ++ "</td><td>" ++ branch ++ "</td><td>" ++ name ++ "</td><td "++ color status ++">"
    ++ status ++ "</td><td><a href=\"" ++ link ++ "\">log</a></td></tr>"

color "SUCCEED" = "style=\"color:green;\""
color "FAILED" = "style=\"color:red;\""
color _ = ""

-- |Gets name of the build (sha1_time) and returns its build data as (repository name, branch name, build status)
buildSummary :: String -> IO (String, String, String, String)
buildSummary dir = do
    config <- getAppConfig
    meta <- fromMetaFile $ buildLogDir config ++ "/" ++ dir ++ "/meta.txt"
    let link = "http://" ++ webPageIP config ++ "/secosci/getlog.php?type=build&hash=" ++ hash meta ++ "&time=" ++ buildTime meta
    return (repoName meta, branch meta, status meta, link)

-- |Gets name of the test result (sha1_time) and returns its build data as (repository name, branch name, test name, passed tests/ all tests)
testSummary :: String -> IO (String, String, String, String, String)
testSummary dir = do
    config <- getAppConfig
    meta' <- fromMetaFile $ testLogDir config ++ "/" ++ dir ++ "/meta.txt"
    let meta = metaData meta'

    let reportPath = testLogDir config ++ "/" ++ dir ++ "/report.txt"
    reportExists <- doesFileExist reportPath

    report <- if reportExists
                then readFile reportPath
                else return ""
    let tests = parseTestRes $ splitWhen (isInfixOf "###############") $ splitOn "\n" report
    let pass= foldl (\ (x,y) b -> (if b then x+1 else x, y+1)) (0,0) $ processReport <$> splitOn "," <$> tests
    let link = "http://" ++ webPageIP config ++ "/secosci/getlog.php?type=test&hash=" ++ hash meta ++ "&time=" ++ testTime meta'
    return (repoName meta, branch meta, testName meta', colorPercents pass, link)

colorPercents :: (Int, Int) -> String
colorPercents (0,0) = "<font style=\"color:red;\">" ++ "NOT PERFORMED" ++ "</font>"
colorPercents (pass, all) = "<font style=\"color:"++ col ++ ";\">" ++ show pass ++ "/" ++ show all ++ "</font>"
    where
    col = "rgb(" ++ show (round (maxCol - passCol)) ++ "," ++ show (round (passCol)) ++ ",0)"
    passCol = (maxCol/fromIntegral(all)) * fromIntegral(pass)

maxCol = 150

processReport :: [String] -> Bool
processReport (_:_:_:_:"TEST_PASS":_) = True
processReport _ = False

parseTestRes :: [[String]] -> [String]
parseTestRes (_:x:_) = x
parseTestRes _ = []
