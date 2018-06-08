{-# LANGUAGE DeriveGeneric #-}

-- |Module for running tests using config file and "Festral.Weles.API".
module Festral.Tests.Test (
    runTest,
    TestRunnerConfig (..),
    parseTest,
    performTestWithConfig
) where

import Data.Aeson
import GHC.Generics
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

data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    , parser:: String
    } deriving (Show, Generic)

instance FromJSON TestConfig
instance ToJSON TestConfig

data TestRunnerConfig = TestRunnerConfig
    { buildLogDir   :: FilePath
    , testLogDir    :: FilePath
    , welesIP       :: String
    , welesPort     :: String
    , welesFilePort :: String
    , yamls         :: [TestConfig]
    } deriving (Show, Generic)

instance FromJSON TestRunnerConfig
instance ToJSON TestRunnerConfig

data TestResParser a = TCT TCTParser deriving Show

instance TestParser (TestResParser a) where
    parse (TCT x) = parse x
    
    fromWelesFiles x = do
        p <- fromWelesFiles x
        return $ TCT p

    fromFile x = do
        p <- fromFile x
        return $ TCT p

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


-- |Get configuration of the test, output files from Weles, build directory and out root directory
-- and creates directory with test logs.
parseTest :: TestConfig -> [(String, String)] -> FilePath -> FilePath -> IO ()
parseTest config outs buildDir outDir = do
    parser <- getParser (parser config) outs

    metaStr <- readFile $ buildDir ++ "/meta.txt"
    let meta = readMeta metaStr
    time <- show <$> getZonedTime
    let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
    let time = year ++ mounth ++ day ++ hour ++ min ++ secs
    tester <- getEffectiveUserName
    let testMeta = MetaTest meta tester tester time
    
    let outDirName = outDir ++ "/" ++ hash meta ++ "_" ++ time
    createDirectory outDirName
    toFile testMeta (outDirName ++ "/meta.txt")
    toFile meta (outDirName ++ "/build.log")

    writeReportFile (parse parser) (outDirName ++ "/report.txt")
    writeFile (outDirName ++ "/tf.log") (concat $ map (\(n,c) -> c) outs)
        

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
    rpms <- getDirectoryContents buildOutDir
    yamlTemplate <- readFile yamlPath
    writeFile (buildOutDir ++ "/test.yml") (generateFromTemplate yamlTemplate $ yamlTemplater (map (\x -> buildOutDir ++ "/" ++ x) rpms))
    jobId <- withCurrentDirectory buildOutDir $ startJob (buildOutDir ++ "/test.yml")
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

        yamlTemplater :: [String] -> TemplateType -> String
        yamlTemplater out (URL url) = "url: '127.0.0.1/secosci/download.php?file=" ++ resolvedName ++ "'"
            where
                resolvedName = if rpmname == [] then "" else head rpmname
                rpmname = take 1 $ filter(isInfixOf url) $ out

