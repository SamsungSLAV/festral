module Festral.Reporter (
    reportHTML,
    formatTextReport
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
import Festral.Template
import Data.List.Utils

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
           ++ "    <h2>Secos CI summary report for "++ time ++ "</h2>\n"
           ++ "    <h2>Build summary:</h2>\n"
           ++ "    ##TEMPLATE_BUILD_TABLE buildTable##\n"
           ++ "    <h2>Test summary:</h2>\n"
           ++ "    ##TEMPLATE_TEST_TABLE testTable##\n"
           ++ "</body>\n"
           ++ "</html>\n"

-- |Generate HTML report file with results given by second parameter
reportHTML :: String -> [String] -> IO String
reportHTML "" dirs = show <$> getZonedTime >>=
    (\time -> reportHTML (defaultHTML time) dirs)
reportHTML src dirs = generateFromTemplate src (templateHTML dirs)

-- |Make text report when every line has a format like passed in first argument.
-- Format string has special characters:
-- %b - board
-- %t - build type
-- %c - commit name
-- %T - build time
-- %C - toolchain
-- %u - builder username
-- %s - build status
-- %h - build hash
-- %o - build output directory
-- %r - name of the repository
-- %B - branch name
-- %l - tester login
-- %L - tester name
-- %e - test time
-- %n - test name
-- %S - test status
-- %R - pass rating passed/all
-- %% - insert % character
formatTextReport :: String -> [String] -> IO [String]
formatTextReport format dirs = do
    metas <- mapM metaByName dirs
    let tests = filter (\ (n,m) -> isTest m) metas
    mapM f tests
    where
        f (n,m) = do
            let str = foldl (\ s (f,o) -> replace f (o m) s) format formats
            (_,_,_,rating,_) <- testSummary n
            return $ replace "%R" rating str

formats =
    [("%b", liftMeta board)
    ,("%t", liftMeta buildType)
    ,("%c", liftMeta commit)
    ,("%T", liftMeta buildTime)
    ,("%C", liftMeta toolchain)
    ,("%u", liftMeta builder)
    ,("%s", liftMeta status)
    ,("%h", liftMeta hash)
    ,("%o", liftMeta outDir)
    ,("%r", liftMeta repoName)
    ,("%B", liftMeta branch)
    ,("%l", tester)
    ,("%L", testerName)
    ,("%e", testTime)
    ,("%n", testName)
    ,("%S", testStatus)
    ,("%%", (\ _ -> "%"))
    ]

makeBuildRow :: (String, String, String, String) -> String
makeBuildRow (repo, branch, status, link)
    = "<tr><td>" ++ repo ++ "</td><td>"
    ++ branch ++ "</td><td "++ color status ++ ">"
    ++ status ++ "</td><td><a href=\"" ++ link ++ "\">log</a></td></tr>"

makeTestRow :: (String, String, String, String, String) -> String
makeTestRow (repo, branch, name, status, link)
    = "<tr><td>" ++ repo ++ "</td><td>" ++ branch
    ++ "</td><td>" ++ name ++ "</td><td "++ color status ++">"
    ++ status ++ "</td><td><a href=\"" ++ link ++ "\">log</a></td></tr>"

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
            ++ "/secosci/getlog?type=build&hash="
            ++ hash meta ++ "&time=" ++ buildTime meta
    return (repoName meta, branch meta, status meta, link)

-- |Gets name of the test result (sha1_time) and returns its build data as
-- (repository name, branch name, test name, passed tests/ all tests, log link)
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
    let tests = parseTestRes $ splitWhen (isInfixOf "###############") $
            splitOn "\n" report
    let pass= foldl (\ (x,y) b -> (if b then x+1 else x, y+1)) (0,0) $
            processReport <$> splitOn "," <$> tests
    let link = "http://" ++ webPageIP config ++ ":" ++ show (webPagePort config)
             ++ "/secosci/getlog?type=test&hash=" ++ hash meta
             ++ "&time=" ++ testTime meta'
    return (repoName meta, branch meta, testName meta',
            percents pass (testStatus meta'), link)

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
           ++ "             <th>Repository</th><th>Branch</th><th>Test name\
           \</th><th>Test result</th><th>Log file</th>\n"
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
