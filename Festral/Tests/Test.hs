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
import Festral.Weles.API hiding (status, name)
import qualified Festral.Weles.API as WJob (status, name)
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
    show (UnknownError x) = "WELES ERROR: " ++ x

-- |Run tests from config for all build directories listed in given string
performForallNewBuilds :: FilePath -> String -> IO ()
performForallNewBuilds _ "" = return ()
performForallNewBuilds conf list = do
    Par.mapM_ (performTestWithConfig conf) $ lines list

-- |Read configuration file from first parameter and build directory from second and make test log from it
performTestWithConfig :: FilePath -> String -> IO ()
performTestWithConfig confPath target = do
    confStr <- LB.readFile confPath
    let Just config = decode confStr :: Maybe [TestConfig]
    appConfig <- getAppConfig
    tests <- runTests config target
    mapM_ (\ testRes -> parseTest testRes (buildLogDir appConfig ++ "/" ++ target) (testLogDir appConfig)) tests

builtInParsers = ["TCT", "XTest"]

-- |Get result of test, build directory and out root directory
-- and creates directory with test logs.
parseTest :: TestResult -> FilePath -> FilePath -> IO ()
parseTest res@(TestResult _ config) buildDir outDir
    | (parser config) `elem` builtInParsers = parseTest' writeWithParser res buildDir outDir
    | otherwise = parseTest' writeWithOwn res buildDir outDir

writeWithParser config outs outDir = do
    par <- getParser (parser config) outs
    writeReportFile par (outDir ++ "/report.txt")

writeWithOwn config outs outDir = do
    handle err $ do
        fileExists <- doesFileExist $ parser config
        when fileExists $ do
            (inp, out, err, _) <- runInteractiveCommand $ parser config
            forkIO $ hPutStr inp $ concat $ map (\(n,c) -> c) outs
            log <- hGetContents out
            writeFile (outDir ++ "/report.txt") log

    where
        err :: SomeException -> IO ()
        err ex = putStrLn (show ex) >> return ()

-- |Writes information about test to the metafile
writeMetaTest status buildDir outDir name time meta = do
    tester <- getEffectiveUserName
    let testMeta = MetaTest meta tester tester time name status
    let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ time

    latestFile <- freshTests
    appendFile latestFile $ hash meta ++ "_" ++ time ++ "\n"

    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

parseTest' writer (TestResult status config) buildDir outDir = do
    meta <- fromMetaFile $ buildDir ++ "/meta.txt"
    tm <- timeStamp
    let pathPrefix = outDir ++ "/" ++ hash meta
    time <- catch ((createDirectory $ pathPrefix ++ "_" ++ tm) >> return tm) (recreate_dir pathPrefix)
    writeMetaTest (show status) buildDir outDir (name config) time meta
    writeLog status time

    where
        writeLog (SegFault outs) t = writeLog (TestSuccess outs) t
        writeLog (TestSuccess outs) time = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ time
            writer config outs outDirName
            writeFile (outDirName ++ "/tf.log") (concat $ map (\(n,c) ->
                                                     "\n------------------ Begin of " ++ n ++ " ------------------\n"
                                                    ++ c ++ "\n"
                                                    ++ "------------------ End of " ++ n ++ "   ------------------\n") outs)
        writeLog (BadJob x) t = do
            meta <- fromMetaFile $ buildDir ++ "/meta.txt"
            let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ t
            writeFile (outDirName ++ "/tf.log") $ show x


-- |Create directory with appended timestamp. If directory exists,
-- wait for second for get new timestamp and retry.
recreate_dir :: FilePath -> IOError -> IO String
recreate_dir path ex
    | isAlreadyExistsError ex = do
        threadDelay 1000000 -- wait for 1 second
        time <- timeStamp
        catch ((createDirectory $ path ++ "_" ++ time) >> return time) (recreate_dir path)
    | isDoesNotExistError ex = do
        time <- timeStamp
        catch ((createDirectoryIfMissing True $ path ++ "_" ++ time) >> return time) (recreate_dir path)
    | otherwise = (putStrLn $ show ex) >> timeStamp

timeStamp = do
    time <- show <$> getZonedTime
    let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
    return $ year ++ mounth ++ day ++ hour ++ min ++ secs

-- |Converts string name of parser from config JSON to the test parser.
getParser :: String -> [(String, String)] -> IO [TestData]
getParser "TCT" testRes = do
    p <- fromWelesFiles testRes "tct-test-ta.log"
    return $ parseTCT p
getParser "XTest" testRes = do
    p <- fromWelesFiles testRes "xtest.log"
    return $ parseXTest p

runTests :: [TestConfig] -> String -> IO [TestResult]
runTests config target = do
    appConfig <- getAppConfig
    metaStr <- readFile $ (buildLogDir appConfig) ++ "/" ++ target ++ "/meta.txt"
    let meta = readMeta metaStr
    let configs = filterConf config meta

    sequence $ map (runTest target) configs

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
        else fmap Just $ generateFromTemplate yamlTemplate $ yamlTemplater buildOutDir

-- |Helper function for use pattern matching for resolve different errors before test start.
-- runTestJob buildStatus (Maybe parsedYaml) buildId
runTestJob :: String -> Maybe String -> String -> IO JobResult
runTestJob _ Nothing _ = return BadYaml
runTestJob "FAILED" _ _ = return BuildFailed
runTestJob "SUCCEED" (Just yaml) buildId = do
    config <- getAppConfig
    buildOutDir <- buildResDir buildId
    handle emptyFileNotExists $ writeFile (buildOutDir ++ "/test.yml") yaml
    jobId <- handle badJob $ withCurrentDirectory buildOutDir $ startJob (buildOutDir ++ "/test.yml")
    return $ getJobId jobId

    where 
        getJobId :: Maybe Int -> JobResult
        getJobId Nothing = StartJobFailed
        getJobId (Just id) = JobId id
runTestJob _ _ _ = return StartJobFailed

-- |Wait for job if it started successfully and return its results after finish
waitForJob :: JobResult -> Int -> Meta -> IO JobResult
waitForJob (JobId jobid) timeout m = do
    putStrLn $ "[" ++ repoName m ++ "][" ++ show jobid ++ "]Waiting for job finished with " ++ show timeout ++ "sec. timeout ..."
    hFlush stdout

    job <- getJobWhenDone jobid timeout
    putStrLn $ "[" ++ repoName m ++ "][" ++ show jobid ++ "]Finished: " ++ show (fmap WJob.status job)
    filesFromJob job >>= outToResults

    where 
        outToResults :: Either [String] JobResult -> IO JobResult
        outToResults (Right x) = return x
        outToResults (Left x) = do
            y <- logs x jobid
            return $ JobLogs y
waitForJob x _ _ = return x

filesFromJob :: Maybe Job -> IO (Either [String] JobResult)
filesFromJob Nothing = return $ Right StartJobFailed
filesFromJob (Just job) = if WJob.status job == "FAILED"
                            then return $ Right (dryadErr job)
                            else do
                                jobFiles <- (filter (not . (isInfixOf ".rpm")) <$>) <$> getFileList (jobid job)
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
    let content' = if isNothing content
                    then ""
                    else fromJust content
    return (fname, content')

-- |Run test for the given build and return pairs (file name, contents) of files created on Weles
-- runTest build_name path_to_config_fiile
runTest :: String -> TestConfig -> IO TestResult
runTest target testConf = do
    config <- getAppConfig
    meta <- fromMetaFile $ (buildLogDir config) ++ "/" ++ target ++ "/meta.txt"
    let yamlPath = yaml testConf
    putStrLn $ "\n[" ++ repoName meta ++ "]Starting Weles job with " ++ yamlPath ++ " ..."
    yaml <- getYaml yamlPath target
    jobId <- runTestJob (status meta) yaml target
    putStrLn $ "[" ++ repoName meta ++ "]Started job with id: " ++ show jobId
    jobRes <- waitForJob jobId 3600 meta
    testResults jobRes meta testConf

testResults :: JobResult -> Meta -> TestConfig -> IO TestResult
testResults BuildFailed m c = do
    putStrLn ("[" ++ repoName m ++ "][NOTE]This repository build failed. Nothing to test.") 
    return $ TestResult (BadJob BuildFailed) c
testResults BadYaml m c = do
    putStrLn $ "[" ++ repoName m ++ "][ERROR]No such YAML testcase file." 
    return $ TestResult (BadJob BadYaml) c
testResults (JobLogs logs) m conf = do
    resLog <- fromWelesFiles logs "results"
    if "Segmentation fault" `isInfixOf` out resLog
        then return $ TestResult (SegFault logs) conf
        else return $ TestResult (TestSuccess logs) conf
testResults err m c = do
    putStrLn $ "[" ++ repoName m ++ "][ERROR][" ++ show err ++ "]"
    return $ TestResult (BadJob err) c

yamlTemplater :: String -> TemplateType -> IO String
yamlTemplater outDir (URI url) = do
    config <- getAppConfig
    rpms <- catch (getDirectoryContents outDir) dirDoesntExists
    let rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $ filter(isInfixOf url) $ rpms
    return $ "uri: 'http://" ++ webPageIP config ++ "/secosci/download.php?file=" ++ resolvedName rpmname ++ "&build=" ++ hash ++ "/" ++ dir ++ "'"
    where
        (dir:hash:_) = reverse $ splitOn "/" outDir

yamlTemplater outDir (Latest_URI url) = do
    config <- getAppConfig
    cachePath <- buildCache
    cache <- catch (readFile cachePath) fileNotExists
    let (cachedName:cachedHash:_) = splitOn "#" $ resolvedName $ sortBy (\a b -> length a `compare` length b) $ filter (isInfixOf url) $ splitOn "\n" cache
    return $ "uri: 'http://"++ webPageIP config ++ "/secosci/download.php?file=" ++ cachedName ++ "&build=" ++ cachedHash ++ "/build_res'"

yamlTemplater outDir (RPMInstallCurrent pkg) = do
    uri <- yamlTemplater outDir (URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater outDir (RPMInstallLatest pkg) = do
    uri <- yamlTemplater outDir (Latest_URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater _ (FileContent fname) = do
    content <- handle fileNotExists $ readFile fname
    return content

yamlTemplaterRpm  uri package =
        "- push:\n"
    ++ "                  " ++ uri ++ "\n"
    ++ "                  dest: '/tmp/" ++ rpmName ++ "'\n"
    ++ "                  alias: '" ++ rpmName ++ "'\n"
    ++ "              - run:\n"
    ++ "                  name: \"'rpm -i /tmp/" ++ rpmName ++ " --force 2>&1 >> /tmp/install.log'\""
    where
        rpmName = package ++ ".rpm"

filterConf config meta = filter (\x -> repo x == (repoName meta)) config

dirDoesntExists :: SomeException -> IO [FilePath]
dirDoesntExists ex = putStrLn (show ex) >> return []

fileNotExists :: SomeException -> IO String
fileNotExists ex = putStrLn (show ex) >> return ""

emptyFileNotExists :: SomeException -> IO ()
emptyFileNotExists ex = putStrLn $ show ex

badJob :: SomeException -> IO (Maybe Int)
badJob ex = putStrLn (show ex) >> return (Nothing)

resolvedName x = if x == [] then "" else head x
