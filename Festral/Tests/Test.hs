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

-- |Module for running tests using config file and "Festral.Weles.API".
module Festral.Tests.Test (
    runTest,
    runTests,
    parseTest,
    performTestWithConfig,
    performForallNewBuilds
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Festral.SLAV.Weles hiding (status, name)
import qualified Festral.SLAV.Weles as WJob (status, name)
import Data.Maybe
import Festral.Builder.Meta hiding (parse, fromFile)
import System.Directory
import Festral.Tests.TestParser
import Data.Time
import System.Posix.User
import Data.List.Split
import Festral.Template
import Data.List
import Control.Exception
import System.IO.Error
import Festral.Config
import System.Process
import System.IO
import qualified Control.Monad.Parallel as Par
import Festral.Files
import Control.Concurrent
import System.FilePath.Posix
import Control.Monad
import System.Console.ANSI
import Control.Concurrent.MVar

-- |List of pairs filename - content
type FileContents = [(String, String)]

data TestResult
    = TestResult
        { testStatus :: TestStatus
        , testConfig :: TestConfig
        }
    deriving Show

data TestStatus
    = SegFault FileContents
    | BadJob JobResult
    | TestSuccess FileContents

instance Show TestStatus where
    show (SegFault _) = "SEGFAULT"
    show (BadJob x) = show x
    show (TestSuccess _) = "COMPLETE"

data JobResult
    = BuildFailed
    | BadYaml
    | StartJobFailed
    | JobId Int
    | JobLogs FileContents
    | DryadError
    | DownloadError
    | UnknownError String

instance Show JobResult where
    show BuildFailed    = "BUILD FAILED"
    show BadYaml        = "YAML NOT FOUND"
    show StartJobFailed = "NO JOB STARTED"
    show (JobId x)      = show x
    show (JobLogs x)    = show x
    show DryadError     = "DEVICE FAILED"
    show DownloadError  = "DOWNLOAD FILES ERROR"
    show (UnknownError x) = "WELES ERROR"

-- |Run tests from config for all build directories listed in given string.
-- Returns list of names of test results directories
performForallNewBuilds :: FilePath -> [String] -> IO [String]
performForallNewBuilds _ [] = return []
performForallNewBuilds conf list = do
    lock <- newMVar ()
    concat <$> Par.mapM (performAsyncTestWithConfig conf lock) list

-- |Read configuration file from first parameter and build directory from
-- second and make test log from it
performTestWithConfig :: FilePath -> String -> IO [String]
performTestWithConfig confpath target = do
    lock <- newMVar ()
    performAsyncTestWithConfig confpath lock target

-- |Perform test with config and MVar for make execution of some internal things
-- synchronized. It is necessary for `performForallNewBuilds`
performAsyncTestWithConfig :: FilePath -> MVar a -> String -> IO [String]
performAsyncTestWithConfig confPath lock target = do
    confStr <- LB.readFile confPath
    let Just config = decode confStr :: Maybe [TestConfig]
    appConfig <- getAppConfig
    tests <- runTestsAsync lock config target
    mapM (\ testRes ->
            parseTest
                testRes
                (buildLogDir appConfig ++ "/" ++ target)
                (testLogDir appConfig))
        tests

builtInParsers = ["Default", "XTest"]
testFileName = "test.log"

-- |Get result of test, build directory and out root directory
-- and creates directory with test logs. Returns name of out directory.
parseTest :: TestResult -> FilePath -> FilePath -> IO String
parseTest res@(TestResult _ config) buildDir outDir
    | (parser config) `elem` builtInParsers
        = parseTest' writeWithParser res buildDir outDir
    | otherwise = parseTest' writeWithOwn res buildDir outDir

writeWithParser config outs outDir = do
    par <- getParser (parser config) outs
    writeReportFile par (outDir ++ "/report.txt")

writeWithOwn config outs outDir = do
    handle err $ do
        fileExists <- doesFileExist $ parser config
        when fileExists $ do
            (inp, out, err, _) <- runInteractiveCommand $ parser config
            forkIO $
                hPutStr inp $
                concat $ map snd $
                filter (\ (n,c) -> n `isInfixOf` testFileName)
                outs
            log <- hGetContents out
            writeFile (outDir ++ "/report.txt") log

    where
        err :: SomeException -> IO ()
        err ex = putStrLn (show ex) >> return ()

-- |Writes information about test to the metafile
writeMetaTest status buildDir outDir name time meta = do
    tester <- getEffectiveUserName
    let testMeta = MetaTest (id $>> meta) tester tester time name status
    let outDirName = outDir ++ "/" ++ hash $>> meta ++ "_" ++ time

    latestFile <- freshTests
    appendFile latestFile $ hash $>> meta ++ "_" ++ time ++ "\n"

    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

parseTest' writer (TestResult status config) buildDir outDir = do
    meta <- fromMetaFile $ buildDir ++ "/meta.txt"
    tm <- timeStamp
    let pathPrefix = outDir ++ "/" ++ hash $>> meta
    time <- catch
                ((createDirectory $ pathPrefix ++ "_" ++ tm) >> return tm)
                (recreate_dir pathPrefix)
    writeMetaTest (show status) buildDir outDir (name config) time meta
    writeLog status time
    return $ hash $>> meta ++ "_" ++ time

    where
        writeLog (SegFault outs) t = writeLog (TestSuccess outs) t
        writeLog (TestSuccess outs) time = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            writer config outs (outDirName meta time)
            writeFile
                ((outDirName meta time) ++ "/tf.log")
                (concat $ map (\(n,c) ->
                    "\n------------------ Begin of "
                    ++ n ++ " ------------------\n"
                    ++ c ++ "\n"
                    ++ "------------------ End of "
                    ++ n ++ "   ------------------\n")
                outs)
        writeLog (BadJob (UnknownError x)) t = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            writeFile ((outDirName meta t) ++ "/tf.log") $ x
        writeLog (BadJob x) t = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            writeFile ((outDirName meta t) ++ "/tf.log") $ show x
        outDirName meta time = outDir ++ "/" ++ hash $>> meta ++ "_" ++ time


-- |Create directory with appended timestamp. If directory exists,
-- wait for second for get new timestamp and retry.
recreate_dir :: FilePath -> IOError -> IO String
recreate_dir path ex
    | isAlreadyExistsError ex = do
        threadDelay 1000000 -- wait for 1 second
        time <- timeStamp
        catch
            ((createDirectory $ path ++ "_" ++ time) >> return time)
            (recreate_dir path)
    | isDoesNotExistError ex = do
        time <- timeStamp
        catch
            ((createDirectoryIfMissing True $ path ++ "_" ++ time)
                >> return time)
            (recreate_dir path)
    | otherwise = (putStrLn $ show ex) >> timeStamp

timeStamp = do
    time <- show <$> getZonedTime
    let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
    return $ year ++ mounth ++ day ++ hour ++ min ++ secs

parserFromName "Default" = parseDefault
parserFromName "XTest" = parseXTest
parserFromName _ = parseDefault

-- |Converts string name of parser from config JSON to the test parser.
getParser :: String -> [(String, String)] -> IO [TestData]
getParser x testRes = do
    p <- fromWelesFiles testRes testFileName
    return $ (parserFromName x) p

runTests :: [TestConfig] -> String -> IO [TestResult]
runTests x y = newMVar () >>= (\ v -> runTestsAsync v x y)

runTestsAsync :: MVar a -> [TestConfig] -> String -> IO [TestResult]
runTestsAsync lock config target = do
    appConfig <- getAppConfig
    metaStr <- readFile $
                (buildLogDir appConfig) ++ "/" ++ target ++ "/meta.txt"
    let meta = readMeta metaStr
    let configs = filterConf config meta

    sequence $ map (runTestAsync lock target) configs

buildResDir buildId = do
    config <- getAppConfig
    return $ (buildLogDir config) ++ "/" ++ buildId ++ "/build_res"

-- |Get parsed yaml from given path to the template and build id
getYaml :: FilePath -> String -> IO (Maybe String)
getYaml templatePath buildID = do
    config <- getAppConfig
    buildOutDir <- buildResDir buildID
    yamlTemplate <- catch (readFile templatePath) fileNotExists
    if yamlTemplate == ""
        then return Nothing
        else fmap Just $
                generateFromTemplate yamlTemplate $
                yamlTemplater buildOutDir

-- |Helper function for use pattern matching for resolve different errors
-- before test start. RunTestJob buildStatus (Maybe parsedYaml) buildId
runTestJob :: String -> Maybe String -> String -> IO JobResult
runTestJob _ Nothing _ = return BadYaml
runTestJob "FAILED" _ _ = return BuildFailed
runTestJob "SUCCEED" (Just yaml) buildId = do
    config <- getAppConfig
    buildOutDir <- buildResDir buildId
    handle emptyFileNotExists $ writeFile (buildOutDir ++ "/test.yml") yaml
    jobId <- handle badJob $
                withCurrentDirectory buildOutDir $
                startJob (buildOutDir ++ "/test.yml")
    return $ getJobId jobId

    where
        getJobId :: Maybe Int -> JobResult
        getJobId Nothing = StartJobFailed
        getJobId (Just id) = JobId id
runTestJob _ _ _ = return StartJobFailed

-- |Wait for job if it started successfully and return its results after finish
waitForJob :: MVar a -> JobResult -> Int -> Meta -> IO JobResult
waitForJob lock (JobId jobid) timeout m = do
    putAsyncLog lock $ do
        putLogColor m Magenta (show jobid)
        putStrLn $ "Waiting for job finished with "
            ++ show timeout ++ "sec. timeout ..."

    job <- getJobWhenDone jobid timeout
    putAsyncLog lock $ do
        putLogColor m Magenta (show jobid)
        colorBoldBrace "FINISHED" Green
        putStrLn $ show (fmap WJob.status job)
    filesFromJob job >>= outToResults

    where
        outToResults :: Either [String] JobResult -> IO JobResult
        outToResults (Right x) = return x
        outToResults (Left x) = do
            y <- logs x jobid
            return $ JobLogs y
waitForJob _ x _ _ = return x

filesFromJob :: Maybe Job -> IO (Either [String] JobResult)
filesFromJob Nothing = return $ Right StartJobFailed
filesFromJob (Just job) = if WJob.status job == "FAILED"
                            then return $ Right (dryadErr job)
                            else do
                                jobFiles <-
                                    (filter (not . (isInfixOf ".rpm")) <$>)
                                    <$> getFileList (jobid job)
                                if isNothing jobFiles
                                    then return $ Right StartJobFailed
                                    else return $ Left $ fromJust jobFiles

-- |Extract tests results from list of the Weles job's filenames
logs :: [String] -> Int -> IO FileContents
logs files jobid = mapM (fileToFileContent jobid) files

dryadErr job
    | info job == "Failed to download all artifacts for the Job" = DownloadError
    | info job == "Failed to execute test on Dryad." = DryadError
    | otherwise = UnknownError $ info job

-- |Converts filename from Weles to pair (filename, contents)
fileToFileContent :: Int -> String -> IO (String, String)
fileToFileContent jobid fname = do
    content <- getJobOutFile jobid fname
    let content' = fromMaybe "" content
    return (fname, content')

-- |Run test for the given build and return pairs (file name, contents) of files
-- created on Weles.
runTest :: String -> TestConfig -> IO TestResult
runTest x y = newMVar () >>= (\ v -> runTestAsync v x y)

runTestAsync :: MVar a -> String -> TestConfig -> IO TestResult
runTestAsync lock target testConf = do
    config <- getAppConfig
    meta <- fromMetaFile $ (buildLogDir config) ++ "/" ++ target ++ "/meta.txt"
    let yamlPath = yaml testConf
    putAsyncLog lock $
        putLog meta $ "Starting Weles job with " ++ yamlPath ++ "..."
    yaml <- getYaml yamlPath target
    jobId <- runTestJob (status $>> meta) yaml target
    jobRes <- waitForJob lock jobId 3600 meta
    testResults lock jobRes meta testConf

testResults :: MVar a -> JobResult -> Meta -> TestConfig -> IO TestResult
testResults lock BuildFailed m c = do
    putAsyncLog lock $ do
        putLogColor m Yellow "NOTE"
        putStrColor Yellow $ "This repository build failed. Nothing to test.\n"
    return $ TestResult (BadJob BuildFailed) c
testResults lock BadYaml m c = do
    putAsyncLog lock $ do
        putLogColor m Red "ERROR"
        putStrColor Red "No such YAML testcase file.\n"
    return $ TestResult (BadJob BadYaml) c
testResults _(JobLogs logs) m conf = do
    resLog <- fromWelesFiles logs "results"
    if "Segmentation fault" `isInfixOf` out resLog
        then return $ TestResult (SegFault logs) conf
        else return $ TestResult (TestSuccess logs) conf
testResults lock err m c = do
    putAsyncLog lock $ do
        putLogColor m Red "ERROR"
        colorBrace (show err) Red
        putStrLn ""
    return $ TestResult (BadJob err) c

putAsyncLog lock f = withMVar lock $ \_ -> f

putStrColor c s = do
    setSGR [SetColor Foreground Vivid c]
    putStr s
    setSGR [Reset]

putLogColor m c y = do
    colorBrace (repoName  $>> m) Blue
    colorBrace (branch $>> m) Blue
    colorBoldBrace (y) c

putLog m y = do
    colorBrace (repoName $>> m) Blue
    colorBrace (branch $>> m) Blue
    putStrLn y

colorBrace x color = do
    putStr "["
    putStrColor color x
    putStr "]"

colorBoldBrace x color = do
    setSGR [SetConsoleIntensity BoldIntensity]
    colorBrace (x) color
    setSGR [Reset]

brace x = "[" ++ x ++ "]"

yamlTemplater :: String -> TemplateType -> IO String
yamlTemplater outDir (URI url) = do
    config <- getAppConfig
    rpms <- catch (getDirectoryContents outDir) dirDoesntExists
    let rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $
            filter(isInfixOf url) $ rpms
    return $ "uri: 'http://"
        ++ webPageIP config ++ ":"
        ++ show (webPagePort config)
        ++ "/secosci/download.php?file="
        ++ resolvedName rpmname
        ++ "&build="
        ++ hash
        ++ "/"
        ++ dir ++ "'"
    where
        (dir:hash:_) = reverse $ splitOn "/" outDir

yamlTemplater outDir (Latest_URI url) = do
    config <- getAppConfig
    cachePath <- buildCache
    cache <- catch (readFile cachePath) fileNotExists
    let (cachedName,cachedHash) = resolvePkg $ splitOn "#" $ resolvedName $
            sortBy (\a b -> length a `compare` length b)
            $ filter (isInfixOf url) $ splitOn "\n" cache
    return $ "uri: 'http://"
        ++ webPageIP config ++ ":"
        ++ show (webPagePort config)
        ++ "/secosci/download.php?file="
        ++ cachedName
        ++ "&build="
        ++ cachedHash
        ++ "/build_res'"
    where
        resolvePkg (x:y:_) = (x,y)
        resolvePkg _ = ("","")

yamlTemplater outDir (RPMInstallCurrent pkg) = do
    uri <- yamlTemplater outDir (URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater outDir (RPMInstallLatest pkg) = do
    uri <- yamlTemplater outDir (Latest_URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater _ (FileContent fname) = do
    content <- handle fileNotExists $ readFile fname
    return content
yamlTemplater _ (ExecLog cmd logfile) = return $
    "- run:\n\
    \                  name: \"'" ++ cmd ++ " 2>&1 >> " ++ logfile ++ "'\""
yamlTemplater _ (Exec cmd) = return $
    "- run:\n\
    \                  name: \"'" ++ cmd ++ "'\""

yamlTemplaterRpm  uri package =
        "- push:\n"
    ++ "                  " ++ uri ++ "\n"
    ++ "                  dest: '/tmp/" ++ rpmName ++ "'\n"
    ++ "                  alias: '" ++ rpmName ++ "'\n"
    ++ "              - run:\n"
    ++ "                  name: \"'rpm -i /tmp/"
    ++ rpmName ++ " --force 2>&1 >> /tmp/install.log'\""
    where
        rpmName = package ++ ".rpm"

filterConf config meta = filter (\x -> repo x == (repoName $>> meta)) config

dirDoesntExists :: SomeException -> IO [FilePath]
dirDoesntExists ex = putStrLn (show ex) >> return []

fileNotExists :: SomeException -> IO String
fileNotExists ex = putStrLn (show ex) >> return ""

emptyFileNotExists :: SomeException -> IO ()
emptyFileNotExists ex = putStrLn $ show ex

badJob :: SomeException -> IO (Maybe Int)
badJob ex = putStrLn (show ex) >> return (Nothing)

resolvedName x = if x == [] then "" else head x
