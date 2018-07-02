module Festral.Builder.Reporter (
    reportHTML
) where

import System.IO
import System.Directory
import Data.List.Split
import Festral.Files
import Festral.Builder.Meta
import Festral.Tests.Config
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
           ++ "    <h2>CI summary report for "++ time ++ "</h2>\n"
           ++ "    <h2>Build summary:</h2>\n"
           ++ "    <table>\n"
           ++ "        <tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Build result</th>\n"
           ++ "        </tr>\n"
           ++ "        " ++ buildTable ++ "\n"
           ++ "    </table>\n"
           ++ "    <h2>Test summary:</h2>\n"
           ++ "    <table>\n"
           ++ "        <tr>\n"
           ++ "             <th>Repository</th><th>Branch</th><th>Test result</th>\n"
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
    let rows = concat $ map makeRow buildSummaries

    testsFile <- freshTests
    tests <- readFile testsFile
    testSummaries <- sequence $ map testSummary $ filter (not . (== "")) $ splitOn "\n" tests
    let testRows = concat $ map makeRow testSummaries

    time <- show <$> getZonedTime
    return $ html time rows testRows

makeRow :: (String, String, String) -> String
makeRow (repo, branch, status) = "<tr><td>" ++ repo ++ "</td><td>" ++ branch ++ "</td><td>" ++ status ++ "</td></tr>"

-- |Gets name of the build (sha1_time) and returns its build data as (repository name, branch name, build status)
buildSummary :: String -> IO (String, String, String)
buildSummary dir = do
    config <- configFile
    confStr <- LB.readFile config
    let Just config = decode confStr :: Maybe TestRunnerConfig

    meta <- fromMetaFile $ buildLogDir config ++ "/" ++ dir ++ "/meta.txt"
    return (repoName meta, branch meta, status meta)

-- |Gets name of the test result (sha1_time) and returns its build data as (repository name, branch name, passed tests/ all tests)
testSummary :: String -> IO (String, String, String)
testSummary dir = do
    config <- configFile
    confStr <- LB.readFile config
    let Just config = decode confStr :: Maybe TestRunnerConfig

    meta <- fromMetaFile $ testLogDir config ++ "/" ++ dir ++ "/meta.txt"
    report <- readFile $ testLogDir config ++ "/" ++ dir ++ "/report.txt"
    let (_:tests:_) = splitWhen (isInfixOf "###############") $ splitOn "\n" report
    let (pass,all) = foldl (\ (x,y) b -> (if b then x+1 else x, y+1)) (0,0) $ processReport <$> splitOn "," <$> tests
    return (repoName meta, branch meta, show pass ++ "/" ++ show all)

processReport :: [String] -> Bool
processReport (_:_:_:_:"TEST_PASS":_) = True
processReport _ = False
