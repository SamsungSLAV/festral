-- |Module for running tests using config file and "Festral.Weles.API".
module Festral.Tests.Test (
    runTest,
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
import Festral.Tests.TCTParser
import Festral.Tests.TestParser
import Data.Time
import System.Posix.User
import Data.List.Split
import Festral.Weles.YamlTemplate
import Data.List
import Control.Exception
import System.IO.Error
import Festral.Tests.Config
import System.Process
import System.IO
import qualified Control.Monad.Parallel as Par

data TestResParser a = TCT TCTParser deriving Show

instance TestParser (TestResParser a) where
    parse (TCT x) = parse x
    
    fromWelesFiles x = do
        p <- fromWelesFiles x
        return $ TCT p

    fromFile x = do
        p <- fromFile x
        return $ TCT p

-- |Run tests from config for all build directories listed in given string
performForallNewBuilds :: FilePath -> String -> IO ()
performForallNewBuilds _ "" = return ()
performForallNewBuilds conf list = do
    Par.mapM_ (performTestWithConfig conf) $ lines list

-- |Read configuration file from first parameter and build directory from second and get test log from it
performTestWithConfig :: FilePath -> String -> IO ()
performTestWithConfig confPath target = do
    confStr <- LB.readFile confPath
    let Just config = decode confStr :: Maybe TestRunnerConfig
    (testConf, testOut) <- runTest config target
    parseTest testConf testOut (buildLogDir config ++ "/" ++ target) (testLogDir config)
    where
        getConfig [] = TestConfig "" "" ""
        getConfig (x:_) = x


builtInParsers = ["TCT"]

-- |Get configuration of the test, output files from Weles, build directory and out root directory
-- and creates directory with test logs.
parseTest :: TestConfig -> [(String, String)] -> FilePath -> FilePath -> IO ()
parseTest config outs buildDir outDir
    | (parser config) == "" = return ()
    | (parser config) `elem` builtInParsers = parseTest' writeWithParser config outs buildDir outDir
    | otherwise = parseTest' writeWithOwn config outs buildDir outDir

writeWithParser config outs buildDir outDir = do
    parser <- getParser (parser config) outs
    writeReportFile (parse parser) (outDir ++ "/report.txt")
    
writeWithOwn config outs buildDir outDir = do
    handle err $ do
        (inp, out, err, handle) <- runInteractiveProcess (parser config) [concat $ map (\(n,c) -> c) outs] Nothing Nothing
        report <- hGetContents out
        waitForProcess handle
        writeFile (outDir ++ "/report.txt") report

    where
        err :: SomeException -> IO ()
        err ex = putStrLn (show ex) >> return ()


parseTest' writer config outs buildDir outDir = do
    metaStr <- readFile $ buildDir ++ "/meta.txt"
    let meta = readMeta metaStr
    time <- show <$> getZonedTime
    let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
    let time = year ++ mounth ++ day ++ hour ++ min ++ secs
    tester <- getEffectiveUserName
    let testMeta = MetaTest meta tester tester time
    
    let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ time
    catch (createDirectory outDirName) (recreate_dir outDirName 0)
    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

    writer config outs buildDir outDirName
    writeFile (outDirName ++ "/tf.log") (concat $ map (\(n,c) -> c) outs)

    where
        recreate_dir :: FilePath -> Int -> IOError -> IO ()
        recreate_dir path i ex 
            | isAlreadyExistsError ex = catch (createDirectory $ path ++ "_" ++ show i) (recreate_dir path (i+1))
            | otherwise = putStrLn $ show ex
        

getParser :: String -> [(String, String)] -> IO (TestResParser a)
getParser _ testRes = do
    p <- fromWelesFiles testRes
    return $ TCT p

-- |Run test for the given build and return pairs (file name, contents) of files created on Weles
-- runTest path_to_config_fiile build_name
runTest :: TestRunnerConfig -> String -> IO (TestConfig, [(String, String)])
runTest config target = do
    metaStr <- readFile $ (buildLogDir config) ++ "/" ++  target ++ "/meta.txt"
    let meta = readMeta metaStr
    let yamlPath = getYaml $ getConf meta

    putStrLn $ "Starting Weles job with " ++ yamlPath ++ " ..."

    let buildOutDir = (buildLogDir config) ++ "/" ++ target ++ "/build_res"
    rpms <- catch (getDirectoryContents buildOutDir) dirDoesntExists
    yamlTemplate <- catch (readFile yamlPath) fileNotExists
    handle emptyFileNotExists $ writeFile (buildOutDir ++ "/test.yml") (generateFromTemplate yamlTemplate $ yamlTemplater rpms buildOutDir)
    jobId <- if status meta == "SUCCEED" && yamlTemplate /= "" 
                then handle badJob $ withCurrentDirectory buildOutDir $ startJob (buildOutDir ++ "/test.yml")
                else return Nothing

    putStrLn $ "Job id is " ++ show jobId
    let jobId' = if isNothing jobId
                        then return (-1)
                        else return $ fromJust jobId
    jobId'' <- jobId'

    putStr "Waiting for job finished ... "
    job <- getJobWhenDone jobId''
    jobFiles <- getFileList jobId''
    putStrLn $ "OK\n Recieved files: " ++ show jobFiles
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
    return (getTestConf $ getConf meta, ret')
    where
        getConf meta = filter (\x -> repo x == (repoName meta)) $ yamls config
        getYaml [] = []
        getYaml (x:_) = yaml x
        getTestConf [] = TestConfig "" "" ""
        getTestConf (x:_) = x

        dirDoesntExists :: SomeException -> IO [FilePath]
        dirDoesntExists ex = putStrLn (show ex) >> return []

        fileNotExists :: SomeException -> IO String
        fileNotExists ex = putStrLn (show ex) >> return ""

        emptyFileNotExists :: SomeException -> IO ()
        emptyFileNotExists ex = putStrLn $ show ex

        badJob :: SomeException -> IO (Maybe Int)
        badJob ex = putStrLn (show ex) >> return (Nothing)

        yamlTemplater :: [String] -> String -> TemplateType -> String
        yamlTemplater out outDir (URL url) = "uri: 'http://127.0.0.1/secosci/download.php?file=" ++ resolvedName ++ "&build=" ++ hash ++ "/" ++ dir ++ "'"
            where
                resolvedName = if rpmname == [] then "" else head rpmname
                rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $ filter(isInfixOf url) $ out
                (dir:hash:_) = reverse $ splitOn "/" outDir

