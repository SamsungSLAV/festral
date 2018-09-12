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
import Festral.Weles.API
import Data.Maybe
import Festral.Builder.Meta hiding (parse, fromFile)
import System.Directory
import Festral.Tests.TestParser
import Data.Time
import System.Posix.User
import Data.List.Split
import Festral.Weles.YamlTemplate
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
    mapM_ (\ (testConf,testOut) -> parseTest testConf testOut (buildLogDir appConfig ++ "/" ++ target) (testLogDir appConfig)) tests


builtInParsers = ["TCT", "XTest"]

-- |Get configuration of the test, output files from Weles, build directory and out root directory
-- and creates directory with test logs.
parseTest :: TestConfig -> [(String, String)] -> FilePath -> FilePath -> IO ()
parseTest config outs buildDir outDir
    | (parser config) == "" = return ()
    | (parser config) `elem` builtInParsers = parseTest' writeWithParser config outs buildDir outDir
    | otherwise = parseTest' writeWithOwn config outs buildDir outDir

writeWithParser config outs buildDir outDir = do
    par <- getParser (parser config) outs
    writeReportFile par (outDir ++ "/report.txt")

writeWithOwn config outs buildDir outDir = do
    handle err $ do
        fileExists <- doesFileExist $ parser config
        when fileExists $ do
            (inp, out, err, handle) <- runInteractiveProcess (parser config) ["\"'" ++ (concat $ map (\(n,c) -> c) outs) ++ "'\""] Nothing Nothing
            report <- hGetContents out
            waitForProcess handle
            writeFile (outDir ++ "/report.txt") report

    where
        err :: SomeException -> IO ()
        err ex = putStrLn (show ex) >> return ()


parseTest' _ _ [] _ _ = return ()
parseTest' writer config outs buildDir outDir = do
    metaStr <- readFile $ buildDir ++ "/meta.txt"
    let meta = readMeta metaStr
    tester <- getEffectiveUserName

    tm <- timeStamp

    let pathPrefix = outDir ++ "/" ++ hash meta
    time <- catch ((createDirectory $ pathPrefix ++ "_" ++ tm) >> return tm) (recreate_dir pathPrefix)

    let testMeta = MetaTest meta tester tester time (name config)

    let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ time

    latestFile <- freshTests
    appendFile latestFile $ hash meta ++ "_" ++ time ++ "\n"

    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

    writer config outs buildDir outDirName
    writeFile (outDirName ++ "/tf.log") (concat $ map (\(n,c) ->
                                             "\n------------------ Begin of " ++ n ++ " ------------------\n"
                                            ++ c ++ "\n"
                                            ++ "------------------ End of " ++ n ++ "   ------------------\n") outs)

    where
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

runTests :: [TestConfig] -> String -> IO [(TestConfig, [(String, String)])]
runTests config target = do
    appConfig <- getAppConfig
    metaStr <- readFile $ (buildLogDir appConfig) ++ "/" ++ target ++ "/meta.txt"
    let meta = readMeta metaStr
    let configs = filterConf config meta

    sequence $ map (runTest target) configs


-- |Run test for the given build and return pairs (file name, contents) of files created on Weles
-- runTest build_name path_to_config_fiile
runTest :: String -> TestConfig -> IO (TestConfig, [(String, String)])
runTest target testConf = do
    config <- getAppConfig
    metaStr <- readFile $ (buildLogDir config) ++ "/" ++ target ++ "/meta.txt"
    let meta = readMeta metaStr
    let yamlPath = yaml testConf

    putStrLn $ "[" ++ repoName meta ++ "]Starting Weles job with " ++ yamlPath ++ " ..."

    let buildOutDir = (buildLogDir config) ++ "/" ++ target ++ "/build_res"
    rpms <- catch (getDirectoryContents buildOutDir) dirDoesntExists
    yamlTemplate <- catch (readFile yamlPath) fileNotExists
    cachePath <- buildCache
    yamlCache <- catch (readFile cachePath) fileNotExists
    generatedYaml <- generateFromTemplate yamlTemplate $ yamlTemplater config rpms buildOutDir yamlCache
    handle emptyFileNotExists $ writeFile (buildOutDir ++ "/test.yml") generatedYaml
    jobId <- if status meta == "SUCCEED" && yamlTemplate /= ""
                then handle badJob $ withCurrentDirectory buildOutDir $ startJob (buildOutDir ++ "/test.yml")
                else return Nothing

    putStrLn $ "[" ++ repoName meta ++ "]Job id is " ++ show jobId
    let jobId' = if isNothing jobId
                        then return (-1)
                        else return $ fromJust jobId
    jobId'' <- jobId'

    putStrLn $ "[" ++ repoName meta ++ "]Waiting for job finished ... "
    hFlush stdout
    job <- getJobWhenDone jobId'' 3600
    jobFiles <- (filter (not . (isInfixOf ".rpm")) <$>) <$> getFileList jobId''
    putStrLn $ "[" ++ repoName meta ++ "]OK. Recieved files: " ++ show jobFiles
    let jobFiles' = if isNothing jobFiles
                        then []
                        else fromJust jobFiles
    let ret = mapM (\fname -> do
                    content <- getJobOutFile jobId'' fname
                    let content' = if isNothing content
                                    then ""
                                    else fromJust content
                    return (fname, content')
                    ) jobFiles'
    ret' <- ret
    return (testConf, ret')

    where
        yamlTemplater :: TestRunnerConfig -> [String] -> String -> String -> TemplateType -> IO String
        yamlTemplater config out outDir cache (URI url) = return $ "uri: 'http://" ++ webPageIP config ++ "/secosci/download.php?file=" ++ resolvedName rpmname ++ "&build=" ++ hash ++ "/" ++ dir ++ "'"
            where
                rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $ filter(isInfixOf url) $ out
                (dir:hash:_) = reverse $ splitOn "/" outDir

        yamlTemplater config out outDir cache (Latest_URI url) = return $ "uri: 'http://"++ webPageIP config ++ "/secosci/download.php?file=" ++ cachedName ++ "&build=" ++ cachedHash ++ "/build_res'"
            where
                (cachedName:cachedHash:_) = splitOn "#" $ resolvedName $ sortBy (\a b -> length a `compare` length b) $ filter (isInfixOf url) $ splitOn "\n" cache

        yamlTemplater config out outDir cache (RPMInstallCurrent pkg) = do
            uri <- yamlTemplater config out outDir cache (URI pkg)
            return $ yamlTemplaterRpm uri pkg
        yamlTemplater config out outDir cache (RPMInstallLatest pkg) = do
            uri <- yamlTemplater config out outDir cache (Latest_URI pkg)
            return $ yamlTemplaterRpm uri pkg
        yamlTemplater _ _ _ _ (FileContent fname) = do
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
