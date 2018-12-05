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

{-# LANGUAGE BangPatterns #-}

-- |Module for running tests using configuration file from "Festral.Config"
-- and "Festral.SLAV.Weles".
module Festral.Tests.Test (
    runTest,
    runTests,
    parseTest,
    performForallNewBuilds,
    TestResult(..),
    TestStatus(..),
    FileContents,
    JobStartResult(..),
    JobExecutionResult(..),
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LBU
import Festral.SLAV.Weles hiding (status, name)
import qualified Festral.SLAV.Weles as WJob (status, name)
import Data.Maybe
import Festral.Meta hiding (parse, fromFile)
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
import System.IO.Temp

-- |List of pairs filename - content
type FileContents = [(String, String)]

data TestResult
    = TestResult
        { testStatus :: TestStatus
        , testConfig :: TestUnit
        }
    deriving Show

data TestStatus
    -- |Appears when segmantation fault detected during testing process.
    -- Contains test execution logs.
    = SegFault FileContents
    -- |Appears when job finished with error
    | BadJob JobExecutionResult
    -- |If test job finished withowt errors, it returns 'TestSuccess'.
    -- It doesn't mean that tests was passed. Contains test logs.
    | TestSuccess FileContents

instance Show TestStatus where
    show (SegFault _) = "SEGFAULT"
    show (BadJob x) = show x
    show (TestSuccess _) = "COMPLETE"

-- |Status of the starting of test job.
data JobStartResult
    -- |Job was not started because repository under test failed to build
    = BuildFailed
    -- |YAML file passed to the Weles does not exist
    | BadYaml
    -- |Job was not able to start by some reason
    | StartJobFailed
    -- |If job was started successfully, its id is returned.
    -- Contains usual job's ID
    | JobId Int

-- |Status of test job execution.
data JobExecutionResult
    -- |After successfull completion of testing logs are returned by this
    -- constructor. Contains logs and job start result.
    = JobLogs FileContents JobStartResult
    -- |Job execution failed because Dryad failed with error. It usually means
    -- some hardware error (connection between MuxPi and DUT were lost | some
    -- commands executed on DUT failed | DUT was flashed with bad OS image etc.)
    -- Contains logs and job start result.
    | DryadError FileContents JobStartResult
    -- |Weles failed to download some files specified in YAML file (maybe link
    -- is invalid or server has no free space). Contains job start result.
    | DownloadError JobStartResult
    -- |Other unexpected error. Contains error message and job start result.
    | UnknownError String JobStartResult
    -- |Connection for the Weles was lost.
    | ConnectionLost JobStartResult
    -- |Job was not started.
    | JobNotStarted JobStartResult
    -- |Job was timed out and cancelled by Weles
    | Cancelled FileContents JobStartResult

instance Show JobStartResult where
    show BuildFailed        = "BUILD FAILED"
    show BadYaml            = "YAML NOT FOUND"
    show StartJobFailed     = "NO JOB STARTED"
    show (JobId x)          = show x

instance Show JobExecutionResult where
    show (JobLogs x _)      = show x
    show (DryadError{})     = "DEVICE FAILED"
    show (DownloadError{})  = "DOWNLOAD FILES ERROR"
    show (UnknownError{})   = "WELES ERROR"
    show (ConnectionLost{}) = "CONNECTION LOST"
    show (JobNotStarted x)  = show x
    show (Cancelled{})      = "CANCELLED"

-- |Run tests from config for all build directories listed in given string.
-- Returns list of names of test results directories.
performForallNewBuilds :: FilePath      -- ^ Tests configuration file
                       -> [String]      -- ^ List of the build names for test
                       -> IO [String]   -- ^ List of names of performed tests
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
    confStr <- safeReadFile confPath
    let Just config = decode (LBU.fromString confStr) :: Maybe [TestConfig]
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

-- |Get result of the test, its build directory and output root directory
-- and creates directory with test logs. Returns name of created subdirectory
-- (name of the test).
parseTest :: TestResult -- ^ Test result
          -> FilePath   -- ^ Directory of the tested build
          -> FilePath   -- ^ Root directory where put test logs
          -> IO String  -- ^ Name of the test's directory under given root
parseTest res@(TestResult _ config) buildDir outDir
    | (parser $ tConfig config) `elem` builtInParsers
        = parseTest' writeWithParser res buildDir outDir
    | otherwise = parseTest' writeWithOwn res buildDir outDir

writeWithParser config outs outDir = do
    par <- getParser (parser $ tConfig config) outs
    writeReportFile par (outDir ++ "/report.txt")

writeWithOwn test outs outDir = do
    handle err $ do
        fileExists <- doesFileExist $ parser $ config
        when fileExists $ do
            (inp, out, err, _) <- runInteractiveCommand $ parser config
            forkIO $
                hPutStr inp $
                concat $ map snd $
                filter (\ (n,c) -> testFileName `isInfixOf` n)
                outs
            log <- hGetContents out
            writeFile (outDir ++ "/report.txt") log

    where
        err :: SomeException -> IO ()
        err ex = putStrLn (show ex) >> return ()
        config = tConfig test

-- |Writes information about test to the metafile
writeMetaTest status buildDir outDir name time meta device = do
    tester <- getEffectiveUserName
    let testMeta = MetaTest (id $>> meta) tester tester time name status device
    let outDirName = outDir ++ "/" ++ hash $>> meta ++ "_" ++ time

    latestFile <- freshTests
    appendFile latestFile $ hash $>> meta ++ "_" ++ time ++ "\n"

    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

parseTest' writer (TestResult status test) buildDir outDir = do
    meta <- fromMetaFile $ buildDir ++ "/meta.txt"
    tm <- timeStamp
    let pathPrefix = outDir ++ "/" ++ hash $>> meta
    time <- catch
                ((createDirectory $ pathPrefix ++ "_" ++ tm) >> return tm)
                (recreate_dir pathPrefix)
    writeMetaTest
        (show status)
        buildDir
        outDir
        (name config)
        time
        meta
        (target test)
    writeLog status time
    return $ hash $>> meta ++ "_" ++ time

    where
        config = tConfig test
        writeLog (SegFault outs) t = writeLog (TestSuccess outs) t
        writeLog (TestSuccess outs) time = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            writer test outs (outDirName meta time)
            writeFile
                ((outDirName meta time) ++ "/tf.log")
                (concat $ map (\(n,c) ->
                    "\n------------------ Begin of "
                    ++ n ++ " ------------------\n"
                    ++ c ++ "\n"
                    ++ "------------------ End of "
                    ++ n ++ "   ------------------\n")
                outs)
        writeLog (BadJob (DryadError log _ )) t = writeLog (TestSuccess log) t
        writeLog (BadJob (Cancelled log _ )) t = writeLog (TestSuccess log) t
        writeLog (BadJob (UnknownError x _)) t = do
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

-- |Run tests for each given configuration for target 'Build' result.
-- Returns list of test results.
runTests :: [TestConfig]    -- ^ List of test configurations
         -> String          -- ^ Build name to be tested
         -> IO [TestResult] -- ^ List of the results of testing
runTests x y = newMVar () >>= (\ v -> runTestsAsync v x y)

runTestsAsync :: MVar a -> [TestConfig] -> String -> IO [TestResult]
runTestsAsync lock config target = do
    appConfig <- getAppConfig
    meta <- fromMetaFile $
                (buildLogDir appConfig) ++ "/" ++ target ++ "/meta.txt"
    let configs = filterConf config meta

    concat <$> Par.mapM (runTestsForRepoAsync lock target) configs

runTestsForRepoAsync :: MVar a -> String -> TestConfig -> IO [TestResult]
runTestsForRepoAsync lock target config = do
    Par.mapM (runTestAsync lock target) $ map (TestUnit config) $ targets config

buildResDir buildId = do
    config <- getAppConfig
    return $ (buildLogDir config) ++ "/" ++ buildId ++ "/build_res"

-- |Get parsed yaml from given path to the template and build id
getYaml :: FilePath -> String -> TestUnit -> IO (Maybe String)
getYaml templatePath buildID test = do
    config <- getAppConfig
    buildOutDir <- buildResDir buildID
    yamlTemplate <- safeReadFile templatePath
    if yamlTemplate == ""
        then return Nothing
        else fmap Just $
                generateFromTemplate (preprocess test yamlTemplate) $
                    yamlTemplater $ TemplaterOpts buildOutDir test

-- |Helper function for use pattern matching for resolve different errors
-- before test start. runTestJob buildStatus (Maybe parsedYaml) buildId
runTestJob :: String -> Maybe String -> String -> IO JobStartResult
runTestJob _ Nothing _ = return BadYaml
runTestJob "FAILED" _ _ = return BuildFailed
runTestJob "SUCCEED" (Just yaml) buildId = do
    config <- getAppConfig
    buildOutDir <- buildResDir buildId
    withTempFile buildOutDir ".yml" $ \ yamlFileName yamlFileHandle -> do
        handle fileError $ hPutStr yamlFileHandle yaml
        -- Force write data to the file by forsing read it
        !forceFileWrite <- hGetContents yamlFileHandle
        !jobId <- handle badJob $
                    startJob yamlFileName
        return $ maybe StartJobFailed JobId jobId

    where
        fileError :: SomeException -> IO ()
        fileError x = putStrLn $ show x
runTestJob _ _ _ = return StartJobFailed

-- |Wait for job if it started successfully and return its results after finish
waitForJob :: JobStartResult -> JobParameters -> IO JobExecutionResult
waitForJob startRes@(JobId jobid) timeout = do
    job <- getJobWhenDone jobid timeout
    filesFromJob job startRes
waitForJob x _ = return $ JobNotStarted x

allowedLogFiles = [".log", "results"]

-- |After job finished this function checks if it was finished by
-- network connection problems or was it done. If it was done successfully,
-- returns 'Left' with job logs, or if it failed, returns 'Right' error.
filesFromJob :: Maybe Job -> JobStartResult -> IO JobExecutionResult
filesFromJob Nothing res = return $ ConnectionLost res
filesFromJob (Just job) res = do
    jobFiles <- fmap (filter (\ x -> any (`isInfixOf` x) allowedLogFiles))
        <$> getFileList (jobid job)
    log <- maybeLog jobFiles (jobid job)
    return $ resultFromStatus res log job

resultFromStatus res log job
    | status == "FAILED" = dryadErr res log job
    | status == "CANCELED" = maybe (ConnectionLost res) (flip Cancelled res) log
    | otherwise = maybe (ConnectionLost res) (flip JobLogs res) log
    where
        status = WJob.status job

maybeLog :: Maybe [String] -> Int -> IO (Maybe FileContents)
maybeLog Nothing _ = return Nothing
maybeLog (Just x) id = Just <$> logs x id

-- |Extract tests results from list of the Weles job's filenames
logs :: [String] -> Int -> IO FileContents
logs files jobid = mapM (fileToFileContent jobid) files

dryadErr res log job
    | info job == "Failed to download all artifacts for the Job"
        = DownloadError res
    | info job == "Failed to execute test on Dryad."
        = maybe (ConnectionLost res) (flip DryadError res) log
    | otherwise =  UnknownError (info job) res

-- |Converts filename from Weles to pair (filename, contents)
fileToFileContent :: Int -> String -> IO (String, String)
fileToFileContent jobid fname = do
    content <- getJobOutFile jobid fname
    let content' = fromMaybe "" content
    return (fname, content')

-- |Run test for the given build and return pairs (file name, contents) of files
-- created on Weles.
runTest :: String -> TestUnit -> IO TestResult
runTest x y = newMVar () >>= (\ v -> runTestAsync v x y)

runTestAsync :: MVar a -> String -> TestUnit -> IO TestResult
runTestAsync lock build test = do
    config <- getAppConfig
    let testConf = tConfig test
    meta <- fromMetaFile $ (buildLogDir config) ++ "/" ++ build ++ "/meta.txt"
    let yamlPath = yaml testConf
    putAsyncLog lock $ do
        putLogColor meta Magenta [(target test)]
        putStrLn $ "Starting Weles job with " ++ yamlPath ++ "..."
    yaml <- getYaml yamlPath build test
    jobId <- runTestJob (status $>> meta) yaml build
    let jobOpts = JobParameters (timeout testConf) (runTTL testConf)

    putAsyncLog lock $ do
        putLogColor meta Magenta [(target test), (show jobId)]
        putStrLn $ "Waiting for job finished with " ++ show jobOpts

    jobRes <- waitForJob jobId jobOpts
    testResults lock jobRes meta test

testResults :: MVar a -> JobExecutionResult -> Meta -> TestUnit -> IO TestResult
testResults lock (JobLogs logs res) m conf = do
    resLog <- fromWelesFiles logs "results"
    putAsyncLog lock $ do
        putLogColor m Magenta [(target conf), (show res)]
        colorBoldBrace "FINISHED" Green
        putStrLn ""
    if "Segmentation fault" `isInfixOf` out resLog
        then return $ TestResult (SegFault logs) conf
        else return $ TestResult (TestSuccess logs) conf

testResults lock err@(JobNotStarted BuildFailed) m c = do
    putAsyncLog lock $ do
        putLogColor m Magenta [(target c)]
        colorBoldBrace "NOTE" Yellow
        putStrColor Yellow $ "This repository build failed. Nothing to test.\n"
    return $ TestResult (BadJob err) c

testResults lock err@(JobNotStarted BadYaml) m c = do
    putAsyncLog lock $ do
        putLogColor m Magenta [(target c)]
        colorBoldBrace "ERROR" Red
        putStrColor Red "No such YAML testcase file.\n"
    return $ TestResult (BadJob err) c

testResults lock err m c = do
    putAsyncLog lock $ do
        putLogColor m Magenta [(target c), (showJobResultId err)]
        colorBoldBrace "ERROR" Red
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
    mapM_ (flip colorBoldBrace c) y

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
    colorBrace x color
    setSGR [Reset]

brace x = "[" ++ x ++ "]"

filterConf config meta = filter (\x -> repo x == (repoName $>> meta)) config

badJob :: SomeException -> IO (Maybe Int)
badJob ex = putStrLn (show ex) >> return (Nothing)

showJobResultId (JobLogs _ i)       = show i
showJobResultId (DryadError _ i)    = show i
showJobResultId (DownloadError i)   = show i
showJobResultId (UnknownError _ i)  = show i
showJobResultId (ConnectionLost i)  = show i
showJobResultId (JobNotStarted i)   = show i
showJobResultId (Cancelled _ i)     = show i
