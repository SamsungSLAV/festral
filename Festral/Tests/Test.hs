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
    getTestResults
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LBU
import Data.Maybe
import System.Directory
import Data.Time
import System.Posix.User
import Data.List.Split
import Data.List
import Control.Exception
import System.IO.Error
import System.Process
import System.IO
import qualified Control.Monad.Parallel as Par
import Control.Concurrent
import System.FilePath.Posix
import Control.Monad
import Control.Concurrent.MVar
import System.IO.Temp

import Festral.Internal.Files
import Festral.Config
import Festral.Template
import Festral.Tests.TestParser
import Festral.Meta hiding (parse, fromFile)
import Festral.SLAV.Weles hiding (status, name)
import qualified Festral.SLAV.Weles as WJob (status, name)
import Festral.Tests.Data
import Festral.Internal.Logger
import Festral.Internal.Preprocessor

-- |Get parsed result of test specified by id. If test was performed
-- successfully returns parsed result, otherwise returns 'Nothing'.
--
-- @since 1.3.4
getTestResults :: AppConfig -> String -> IO (Maybe String)
getTestResults appConf id = do
    handle badFile $ Just <$> (readFile $ reportFilePath appConf id)
    where
        badFile :: SomeException -> IO (Maybe String)
        badFile _ = return Nothing

-- |Run tests from config for all build directories listed in given string.
-- Returns list of names of test results directories.
performForallNewBuilds :: AppConfig     -- ^ Program configuration
                       -> FilePath      -- ^ Tests configuration file
                       -> [String]      -- ^ List of the build names for test
                       -> IO [String]   -- ^ List of names of performed tests
performForallNewBuilds _ _ [] = return []
performForallNewBuilds appConf conf list = do
    lock <- newMVar ()
    concat <$> Par.mapM (performAsyncTestWithConfig appConf conf lock) list

-- |Read configuration file from first parameter and build directory from
-- second and make test log from it
performTestWithConfig :: AppConfig -> FilePath -> String -> IO [String]
performTestWithConfig appConf confpath target = do
    lock <- newMVar ()
    performAsyncTestWithConfig appConf confpath lock target

-- |Perform test with config and MVar for make execution of some internal things
-- synchronized. It is necessary for `performForallNewBuilds`
performAsyncTestWithConfig :: AppConfig -- ^ Program configuration
                           -> FilePath  -- ^ Path to the test configuration
                           -> MVar a    -- ^ Synchronization variable
                           -> String    -- ^ Name of the build to be tested
                           -> IO [String] -- ^ List of names of performed tests
performAsyncTestWithConfig appConfig confPath lock target = do
    confStr <- safeReadFile confPath
    let Just config = decode (LBU.fromString confStr) :: Maybe [TestConfig]
    tests <- runTestsAsync appConfig lock config target
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
runTests :: AppConfig       -- ^ Program configuration
         -> [TestConfig]    -- ^ List of test configurations
         -> String          -- ^ Build name to be tested
         -> IO [TestResult] -- ^ List of the results of testing
runTests c x y = newMVar () >>= (\ v -> runTestsAsync c v x y)

runTestsAsync :: AppConfig
              -> MVar a
              -> [TestConfig]
              -> String
              -> IO [TestResult]
runTestsAsync appConfig lock config target = do
    meta <- fromMetaFile $
                (buildLogDir appConfig) ++ "/" ++ target ++ "/meta.txt"
    let configs = filterConf config meta

    concat <$> Par.mapM (runTestsForRepoAsync appConfig lock target) configs

runTestsForRepoAsync :: AppConfig
                     -> MVar a
                     -> String
                     -> TestConfig
                     -> IO [TestResult]
runTestsForRepoAsync appConfig lock target config = do
    Par.mapM (runTestAsync appConfig lock target) $
        map (TestUnit config) $ targets config

buildResDir config buildId = do
    return $ (buildLogDir config) ++ "/" ++ buildId ++ "/build_res"

-- |Get parsed yaml from given path to the template and build id
getYaml :: AppConfig -> FilePath -> String -> TestUnit -> IO (Maybe String)
getYaml config templatePath buildID test = do
    buildOutDir <- buildResDir config buildID
    yamlTemplate <- safeReadFile templatePath
    if yamlTemplate == ""
        then return Nothing
        else fmap Just $
                generateFromTemplate (preprocess test yamlTemplate) $
                    yamlTemplater $ TemplaterOpts buildOutDir test
                    $ simpleAddress (webPageIP config) (webPagePort config)

-- |Helper function for use pattern matching for resolve different errors
-- before test start. runTestJob buildStatus (Maybe parsedYaml) buildId
runTestJob :: AppConfig -> String -> Maybe String -> String -> IO JobStartResult
runTestJob _ _ Nothing _ = return BadYaml
runTestJob _ "FAILED" _ _ = return BuildFailed
runTestJob config "SUCCEED" (Just yaml) buildId = do
    let addr = welesAddr config
    buildOutDir <- buildResDir config buildId
    handle createTempFileError
        $ withTempFile buildOutDir ".yml" $ \ yamlFileName yamlFileHandle -> do
            handle fileError $ hPutStr yamlFileHandle yaml
            -- Force write data to the file by forsing read it
            !forceFileWrite <- hGetContents yamlFileHandle
            !jobId <- handle badJob $
                    startJob addr yamlFileName
            return $ maybe StartJobFailed JobId jobId

    where
        fileError :: SomeException -> IO ()
        fileError x = putStrLn $ show x

        createTempFileError :: SomeException -> IO JobStartResult
        createTempFileError x = return BadYaml
runTestJob _ _ _ _ = return StartJobFailed

-- |Wait for job if it started successfully and return its results after finish
waitForJob :: AppConfig
           -> JobStartResult
           -> JobParameters
           -> IO JobExecutionResult
waitForJob config startRes@(JobId jobid) timeout = do
    let addr = welesAddr config
    job <- getJobWhenDone addr jobid timeout
    filesFromJob config job startRes
waitForJob _ x _ = return $ JobNotStarted x

allowedLogFiles = [".log", "results"]

-- |After job finished this function checks if it was finished by
-- network connection problems or was it done. If it was done successfully,
-- returns 'Left' with job logs, or if it failed, returns 'Right' error.
filesFromJob :: AppConfig
             -> Maybe Job
             -> JobStartResult
             -> IO JobExecutionResult
filesFromJob _ Nothing res = return $ ConnectionLost res
filesFromJob config (Just job) res = do
    let addr = welesAddr config
    jobFiles <- fmap (filter (\ x -> any (`isInfixOf` x) allowedLogFiles))
        <$> getFileList addr (jobid job)
    log <- maybeLog config jobFiles (jobid job)
    return $ resultFromStatus res log job

resultFromStatus res log job
    | status == "FAILED" = dryadErr res log job
    | status == "CANCELED" = maybe (ConnectionLost res) (flip Cancelled res) log
    | otherwise = maybe (ConnectionLost res) (flip JobLogs res) log
    where
        status = WJob.status job

maybeLog :: AppConfig -> Maybe [String] -> Int -> IO (Maybe FileContents)
maybeLog _ Nothing _ = return Nothing
maybeLog c (Just x) id = Just <$> logs c x id

-- |Extract tests results from list of the Weles job's filenames
logs :: AppConfig -> [String] -> Int -> IO FileContents
logs c files jobid = mapM (fileToFileContent c jobid) files

dryadErr res log job
    | info job == "Failed to download all artifacts for the Job"
        = DownloadError res
    | info job == "Failed to execute test on Dryad."
        = maybe (ConnectionLost res) (flip DryadError res) log
    | otherwise =  UnknownError (info job) res

-- |Converts filename from Weles to pair (filename, contents)
fileToFileContent :: AppConfig -> Int -> String -> IO (String, String)
fileToFileContent config jobid fname = do
    let addr = welesAddr config
    content <- getJobOutFile addr jobid fname
    let content' = fromMaybe "" content
    return (fname, content')

-- |Run test for the given build and return pairs (file name, contents) of files
-- created on Weles.
runTest :: AppConfig -> String -> TestUnit -> IO TestResult
runTest c x y = newMVar () >>= (\ v -> runTestAsync c v x y)

runTestAsync :: AppConfig -> MVar a -> String -> TestUnit -> IO TestResult
runTestAsync config lock build test = do
    let testConf = tConfig test
    meta <- fromMetaFile $ (buildLogDir config) ++ "/" ++ build ++ "/meta.txt"
    let yamlPath = yaml testConf
    putAsyncLog lock $ do
        putLogColor meta Magenta [(target test)]
        putStrLn $ "Starting Weles job with " ++ yamlPath ++ "..."
    yaml <- getYaml config yamlPath build test
    jobId <- runTestJob config (status $>> meta) yaml build
    let jobOpts = JobParameters (timeout testConf) (runTTL testConf)

    putAsyncLog lock $ do
        putLogColor meta Magenta [(target test), (show jobId)]
        putStrLn $ "Waiting for job finished with " ++ show jobOpts

    jobRes <- waitForJob config jobId jobOpts
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

filterConf config meta = filter (\x -> repo x == (repoName $>> meta)) config

badJob :: SomeException -> IO (Maybe Int)
badJob ex = putStrLn (show ex) >> return (Nothing)

welesAddr c = NetAddress (welesIP c) (welesPort c) (welesFilePort c)
